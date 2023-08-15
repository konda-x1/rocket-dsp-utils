package dspblocks

import chisel3.{Bundle, Data, Module}
import chisel3.iotesters.{PeekPokeTester, Pokeable}
import chisel3.util.IrrevocableIO
import chiseltest.ChiselScalatestTester
import freechips.rocketchip.amba.axi4.{AXI4Bundle, AXI4BundleAR, AXI4BundleAW, AXI4BundleB, AXI4BundleR, AXI4BundleW, AXI4MasterModel}
import freechips.rocketchip.amba.axi4stream.{AXI4StreamBundle, AXI4StreamModel, AXI4StreamTransaction, AXI4StreamTransactionExpect}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.system.BaseConfig
import freechips.rocketchip.util.BundleMap
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.chipsalliance.cde.config.Parameters

class AXI4StreamPeekPokeMasterChiseltest(port: AXI4StreamBundle, tester: chiseltest.iotesters.PeekPokeTester[_]) {
  protected var input: Seq[AXI4StreamTransaction] = Seq()

  def addTransactions(in: Seq[AXI4StreamTransaction]): Unit = {
    input ++= in
  }

  def step(): Unit = {
    import tester.{peek, poke}
    if (input.isEmpty) {
      poke(port.valid, 0)
    } else {
      val t = input.head
      poke(port.valid, 1)
      poke(port.bits.data, t.data)
      poke(port.bits.last, if (t.last) 1 else 0)
      if (port.bits.strb.getWidth > 0) {
        if (t.strb == -1) {
          val allOnes = (BigInt(1) << port.bits.strb.getWidth) - 1
          poke(port.bits.strb, allOnes)
        } else {
          poke(port.bits.strb, t.strb)
        }
      }
      if (port.bits.keep.getWidth > 0) {
        if (t.keep == -1) {
          val allOnes = (BigInt(1) << port.bits.keep.getWidth) - 1
          poke(port.bits.keep, allOnes)
        } else {
          poke(port.bits.keep, t.keep)
        }
      }
      if (port.bits.user.getWidth > 0) {
        poke(port.bits.user, t.user)
      }
      if (port.bits.id.getWidth > 0) {
        poke(port.bits.id, t.id)
      }
      if (port.bits.dest.getWidth > 0) {
        poke(port.bits.dest, t.dest)
      }
      if (peek(port.ready) != BigInt(0)) {
        input = input.tail
      }
    }
  }

  def complete(): Boolean = {
    input.isEmpty
  }
}


class AXI4StreamPeekPokeSlaveChiseltest(port: AXI4StreamBundle, tester: chiseltest.iotesters.PeekPokeTester[_]) {

  protected var output: Seq[AXI4StreamTransaction] = Seq()
  protected var expects: Seq[AXI4StreamTransactionExpect] = Seq()

  def addExpects(expect: Seq[AXI4StreamTransactionExpect]): Unit = {
    expects ++= expect
  }

  def getTransactions(): Seq[AXI4StreamTransaction] = {
    val toret = output
    output = Seq()
    toret
  }

  def expect(port: AXI4StreamBundle, value: AXI4StreamTransactionExpect): Boolean = {
    import tester.{expect => texpect}
    value.data.forall(texpect(port.bits.data, _)) &&
      value.last.forall { x => texpect(port.bits.last, if (x) 1 else 0) } &&
      value.strb.forall(texpect(port.bits.strb, _)) &&
      value.keep.forall(texpect(port.bits.keep, _)) &&
      value.user.forall(texpect(port.bits.user, _)) &&
      value.id.forall(texpect(port.bits.id, _)) &&
      value.dest.forall(texpect(port.bits.dest, _))
  }

  def step(): Unit = {
    import tester.{peek, poke}
    if (expects.isEmpty) {
      poke(port.ready, 0)
    } else {
      poke(port.ready, 1)
      if (peek(port.valid) != BigInt(0)) {
        expect(port, expects.head)
        expects = expects.tail

        val data = if (port.params.hasData && port.params.n > 0) {
          peek(port.bits.data)
        } else {
          BigInt(0)
        }
        val last = peek(port.bits.last) != BigInt(0)
        val strb = if (port.params.hasStrb && port.params.n > 0) {
          peek(port.bits.strb)
        } else {
          BigInt(-1)
        }
        val keep = if (port.params.hasKeep && port.params.n > 0) {
          peek(port.bits.keep)
        } else {
          BigInt(-1)
        }
        val user = if (port.params.u > 0) {
          peek(port.bits.user)
        } else {
          BigInt(0)
        }
        val id = if (port.params.i > 0) {
          peek(port.bits.id)
        } else {
          BigInt(0)
        }
        val dest = if (port.params.d > 0) {
          peek(port.bits.dest)
        } else {
          BigInt(0)
        }

        output +:= AXI4StreamTransaction(
          data = data,
          last = last,
          strb = strb,
          keep = keep,
          user = user,
          id = id,
          dest = dest
        )
      }
    }
  }

  def complete(): Boolean = {
    expects.isEmpty
  }
}

trait AXI4StreamMasterModelChiseltest[T <: Module] extends chiseltest.iotesters.PeekPokeTester[T] {
  protected var masters: Seq[AXI4StreamPeekPokeMasterChiseltest] = Seq()

  def resetMaster(port: AXI4StreamBundle): Unit = {
    poke(port.valid, 0)
  }

  def bindMaster(port: AXI4StreamBundle): AXI4StreamPeekPokeMasterChiseltest = {
    resetMaster(port)
    val master = new AXI4StreamPeekPokeMasterChiseltest(port, this)
    masters +:= master
    master
  }

  protected def stepMasters(): Unit = {
    masters.foreach(_.step())
  }

  override def step(n: Int): Unit = {
    for (_ <- 0 until n) {
      stepMasters()
      super.step(1)
    }
  }

  def mastersComplete(): Boolean = {
    masters.map(_.complete()).forall(x => x)
  }

  def stepToCompletion(maxCycles: Int = 1000, silentFail: Boolean = false): Unit = {
    for (_ <- 0 until maxCycles) {
      if (mastersComplete()) {
        step(1)
        return
      } else {
        step(1)
      }
    }
    require(silentFail, s"slavesComplete: ${mastersComplete()}")
  }
}

trait AXI4StreamSlaveModelChiseltest[T <: Module] extends chiseltest.iotesters.PeekPokeTester[T] {
  protected var slaves: Seq[AXI4StreamPeekPokeSlaveChiseltest] = Seq()

  def resetSlave(port: AXI4StreamBundle): Unit = {
    poke(port.ready, 0)
  }

  def bindSlave(port: AXI4StreamBundle): AXI4StreamPeekPokeSlaveChiseltest = {
    resetSlave(port)
    val slave = new AXI4StreamPeekPokeSlaveChiseltest(port, this)
    slaves +:= slave
    slave
  }

  protected def stepSlaves(): Unit = {
    slaves.foreach(_.step())
  }

  override def step(n: Int): Unit = {
    for (_ <- 0 until n) {
      stepSlaves()
      super.step(1)
    }
  }

  def stepToCompletion(maxCycles: Int = 1000, silentFail: Boolean = false): Unit = {
    for (_ <- 0 until maxCycles) {
      if (slavesComplete()) {
        step(1)
        return
      } else {
        step(1)
      }
    }
    require(silentFail, s"slavesComplete: ${slavesComplete()}")
  }

  def slavesComplete(): Boolean = {
    slaves.map(_.complete()).forall(x => x)
  }
}

trait AXI4StreamModelChiseltest[T <: Module] extends
  AXI4StreamSlaveModelChiseltest[T] with AXI4StreamMasterModelChiseltest[T] {

  override def step(n: Int): Unit = {
    for (_ <- 0 until n) {
      stepMasters()
      super[AXI4StreamSlaveModelChiseltest].step(1)
    }
  }

  override def stepToCompletion(maxCycles: Int = 1000, silentFail: Boolean = false): Unit = {
    for (_ <- 0 until maxCycles) {
      if (slavesComplete() && mastersComplete()) {
        step(1)
        return
      } else {
        step(1)
      }
    }
    require(silentFail, s"stepToCompletion failed at $maxCycles cycles")
  }
}

abstract class PassthroughChiseltestTester[D, U, EO, EI, B <: Data](dut: Passthrough[D, U, EO, EI, B] with StandaloneBlock[D, U, EO, EI, B])
  extends chiseltest.iotesters.PeekPokeTester(dut.module) with MemTester with AXI4StreamModelChiseltest[LazyModuleImp] {
  resetMem()

  val in = dut.in.getWrappedValue
  val out = dut.out.getWrappedValue

  val master = bindMaster(in)
  val slave = bindSlave(out)

  step(5)

  val depth = readAddr(BigInt(0)).toInt
  val expectedDepth = dut.params.depth
  expect(depth == expectedDepth, s"Depth was $depth, should be $expectedDepth")

  // fill queue
  master.addTransactions((0 until expectedDepth).map(x => AXI4StreamTransaction(data = x)))
  stepToCompletion()

  // queue should be full
  expect(in.ready, 0)
  expect(out.valid, 1)

  // empty queue
  slave.addExpects((0 until expectedDepth).map(x => AXI4StreamTransactionExpect(data = Some(x))))
  stepToCompletion()

  // should be done
  expect(out.valid, 0)
}

trait AXI4MasterModelChiseltest extends MemMasterModel {
  self: chiseltest.iotesters.PeekPokeTester[_] =>
  import AXI4MasterModel._

  def memAXI: AXI4Bundle

  def maxWait = 500

  def fire(io: IrrevocableIO[_]): Boolean = {
    (peek(io.valid) != BigInt(0)) && (peek(io.ready) != BigInt(0))
  }

  def pokeUser(user: BundleMap, values: Map[String, BigInt]): Unit = {
    for ( (k, v) <- values ) {
      user.elements.get(k) match {
        case Some(chiseltest.iotesters.Pokeable(e)) =>
          poke(e, v)
        case Some(b: Bundle) =>
          poke(b, values)
        case Some(d) =>
          println(s"Don't know how to poke element $d")
        case None =>
          println(s"user field $k not found")
      }
    }
  }

  def pokeAW(aw: AXI4BundleAW, value: AWChannel): Unit = {
    poke(aw.id,     value.id)
    poke(aw.addr,   value.addr)
    poke(aw.len,    value.len)
    poke(aw.size,   value.size)
    poke(aw.burst,  value.burst)
    poke(aw.lock,   value.lock)
    poke(aw.cache,  value.cache)
    poke(aw.prot,   value.prot)
    poke(aw.qos,    value.qos)
    // poke(aw., value.region)
    require(value.region == BigInt(0), s"region is optional and rocket-chip left it out. overriding the default value here with ${value.region} won't do anything")
    pokeUser(aw.user, value.user)
  }


  def pokeAR(ar: AXI4BundleAR, value: ARChannel): Unit = {
    poke(ar.id, value.id)
    poke(ar.addr, value.addr)
    poke(ar.len, value.len)
    poke(ar.size, value.size)
    poke(ar.burst, value.burst)
    poke(ar.lock, value.lock)
    poke(ar.cache, value.cache)
    poke(ar.prot, value.prot)
    poke(ar.qos,  value.qos)
    pokeUser(ar.user, value.user)
  }

  def pokeW(w: AXI4BundleW, value: WChannel): Unit = {
    poke(w.data, value.data)
    poke(w.strb, value.strb)
    poke(w.last, value.last)
  }

  def peekR(r: AXI4BundleR): RChannel = {
    RChannel(
      id   = peek(r.id),
      data = peek(r.data),
      resp = peek(r.resp),
      last = peek(r.last),
      user = r.user.elements.map { case (n: String, chiseltest.iotesters.Pokeable(d)) => n -> peek(d) }
    )
  }

  def peekB(b: AXI4BundleB): BChannel = {
    BChannel(
      id = peek(b.id),
      resp = peek(b.resp),
      user = b.user.elements.map { case (n: String, chiseltest.iotesters.Pokeable(d)) => n -> peek(d) }
    )
  }

  def memWriteWord(addr: BigInt, data: BigInt): Unit = axiWriteWord(addr, data)
  def axiWriteWord(addr: BigInt, data: BigInt): Unit = {
    val awChannel = AWChannel(
      addr = addr,
      size = 3        // 8 bytes
    )
    val wChannel  = WChannel(
      data = data,
      strb = 0xFF,    // 8 bytes
      last = true    // one word only
    )

    // poke AW and W channels
    pokeAW(memAXI.aw.bits, awChannel)
    pokeW(memAXI.w.bits, wChannel)

    var awFinished = false
    var wFinished = false
    var cyclesWaited = 0

    poke(memAXI.aw.valid, 1)
    poke(memAXI.w.valid,  1)

    while (!awFinished || !wFinished) {
      if (!awFinished) { awFinished = fire(memAXI.aw) }
      if (! wFinished) {  wFinished = fire(memAXI.w)  }
      require(cyclesWaited < maxWait || awFinished, s"Timeout waiting for AW to be ready ($maxWait cycles)")
      require(cyclesWaited < maxWait || wFinished,  s"Timeout waiting for W to be ready ($maxWait cycles)")
      // if (cyclesWaited >= maxWait) {
      //   return
      // }
      cyclesWaited += 1
      step(1)
      if (awFinished) { poke(memAXI.aw.valid, 0) }
      if ( wFinished) { poke(memAXI.w.valid,  0) }
    }

    // wait for resp
    cyclesWaited = 0
    poke(memAXI.b.ready, 1)
    var bFinished = false
    var b = peekB(memAXI.b.bits)

    while (!bFinished) {
      bFinished = peek(memAXI.b.valid) != BigInt(0)
      b = peekB(memAXI.b.bits)
      require(cyclesWaited < maxWait, s"Timeout waiting for B to be valid ($maxWait cycles)")
      step(1)
      cyclesWaited += 1
    }

    poke(memAXI.b.ready, 0)

    require(b.id == awChannel.id, s"Got bad id (${b.id} != ${awChannel.id})")
    require(b.resp == BRESP_OKAY, s"BRESP not OKAY (got ${b.resp}")

  }
  def memReadWord(addr: BigInt) = axiReadWord(addr)
  def axiReadWord(addr: BigInt): BigInt = {
    val arChannel = ARChannel(
      addr = addr,
      size = 3        // 8 bytes
    )

    pokeAR(memAXI.ar.bits, arChannel)
    poke(memAXI.ar.valid, 1)

    var cyclesWaited = 0
    var arFinished = false

    while (!arFinished) {
      arFinished = peek(memAXI.ar.ready) != BigInt(0)
      require(cyclesWaited < maxWait, s"Timeout waiting for AR to be ready ($maxWait cycles)")
      /* if (cyclesWaited >= maxWait) {
        println(s"Timeout waiting for AR to be ready ($maxWait cycles)")
        arFinished = true
      } */
      step(1)
      cyclesWaited += 1
    }

    poke(memAXI.ar.valid, 0)
    poke(memAXI.r.ready, 1)
    //step(1)

    var rFinished = false
    cyclesWaited = 0
    var rChannel = peekR(memAXI.r.bits)

    while (!rFinished) {
      poke(memAXI.ar.valid, 0)
      rFinished = peek(memAXI.r.valid) != BigInt(0)
      if (rFinished) {
        rChannel = peekR(memAXI.r.bits)
      }
      step(1)
      require(cyclesWaited < maxWait, s"Timeout waiting for R to be valid ($maxWait cycles)")
      /* if (cyclesWaited >= maxWait) {
        println(s"Timeout waiting for R to be ready ($maxWait cycles)")
        rFinished = true // hack hack hack
      } */

      cyclesWaited += 1
    }

    poke(memAXI.r.ready, 0)

    require(rChannel.last != BigInt(0))
    require(rChannel.id == arChannel.id, s"Got id ${rChannel.id} instead of ${arChannel.id}")
    require(rChannel.resp == RRESP_OKAY, s"RRESP not OKAY (got ${rChannel.resp}")
    rChannel.data
  }

  def axiReset(): Unit = {
    pokeAR(memAXI.ar.bits, ARChannel())
    pokeAW(memAXI.aw.bits, AWChannel())
    pokeW(memAXI.w.bits, WChannel())
    poke(memAXI.ar.valid, 0)
    poke(memAXI.aw.valid, 0)
    poke(memAXI.w.valid, 0)
    poke(memAXI.r.ready, 0)
    poke(memAXI.b.ready, 0)
  }
}

trait AXI4MemTesterChiseltest extends AXI4MasterModelChiseltest {
  self: chiseltest.iotesters.PeekPokeTester[_] =>
  def resetMem(): Unit = {
    axiReset()
  }

  def readAddr(addr: BigInt): BigInt = {
    axiReadWord(addr)
  }

  def writeAddr(addr: BigInt, value: BigInt): Unit = {
    axiWriteWord(addr, value)
  }
}

class AXI4PassthroughChiseltestTester(c: AXI4Passthrough with AXI4StandaloneBlock)
  extends PassthroughChiseltestTester(c) with AXI4MemTesterChiseltest {
  def memAXI = c.ioMem.get.getWrappedValue
}

class BrokenSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  implicit val p: Parameters = (new BaseConfig).toInstance

  behavior of "Broken"

  it should "just work" in {
    val lazymod = LazyModule(new AXI4Passthrough(PassthroughParams(1)) with AXI4StandaloneBlock)
    val dut = () => lazymod.module

    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), dut) {
      c => new AXI4PassthroughTester(lazymod)
    } should be(true)
  }

  it should "just work (chiseltest)" in {
    val lazymod = LazyModule(new AXI4Passthrough(PassthroughParams(1)) with AXI4StandaloneBlock)
//    val dut = () => lazymod.module
//
//    chisel3.iotesters.Driver.execute(Array("-tbn", "firrtl", "-fiwv"), dut) {
//      c => new AXI4PassthroughTester(lazymod)
//    } should be(true)
    test(lazymod.module).runPeekPoke(_ => new AXI4PassthroughChiseltestTester(lazymod))
  }
}

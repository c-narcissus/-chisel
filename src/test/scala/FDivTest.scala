import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class FDivTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDiv"

  it should "correctly divide two floating-point numbers" in {
    test(new FDiv) { c =>
      // Initialize the input
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)  // positive number
      c.io.a.exp.poke("b01111110".U) // exponent = 126
      c.io.a.significand.poke("b10000000000000000000000".U)

      c.io.b.sign.poke(0.U)  // positive number
      c.io.b.exp.poke("b01111110".U) // exponent = 126
      c.io.b.significand.poke("b00000000000000000000000".U)

      // Wait until the module is ready
      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1) // additional step to ensure stability of output

      // Check the outputs after the computation is ready
      c.io.out.sign.expect(0.U) // Result should be positive
     
      c.io.out.significand.expect("b10000000000000000000000".U) // Expected significand
       c.io.out.exp.expect("b01111111".U) // Expected exponent = 127 (normalized)
      c.io.ready.expect(true.B) // Ensure that the operation is indeed completed
      c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "handle division by zero" in {
    test(new FDiv) { c =>
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b01111110".U) // Non-zero
      c.io.a.significand.poke("b00000000000000000000000".U)

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b00000000".U) // Zero
      c.io.b.significand.poke("b00000000000000000000000".U)

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1)

      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b11111111".U) // Infinity
      c.io.out.significand.expect("b00000000000000000000000".U)
      c.io.ready.expect(true.B)
      c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "handle NaN and infinity scenarios" in {
    test(new FDiv) { c =>
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(0.U)
      c.io.a.exp.poke("b11111111".U) // Infinity
      c.io.a.significand.poke("b00000000000000000000000".U)

      c.io.b.sign.poke(0.U)
      c.io.b.exp.poke("b11111111".U) // NaN
      c.io.b.significand.poke("b00000000000000000000001".U) // Non-zero significand for NaN

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1)

      c.io.out.sign.expect(0.U)
      c.io.out.exp.expect("b11111111".U) // NaN
      c.io.out.significand.expect("b00000000000000000000001".U) // Non-zero significand
      c.io.ready.expect(true.B)
      c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  it should "correctly handle the division of negative and positive numbers" in {
    test(new FDiv) { c =>
      c.io.valid.poke(true.B)
      c.io.a.sign.poke(1.U)  // negative number
      c.io.a.exp.poke("b01111110".U)
      c.io.a.significand.poke("b00000000000000000000000".U)

      c.io.b.sign.poke(0.U)  // positive number
      c.io.b.exp.poke("b01111110".U)
      c.io.b.significand.poke("b00000000000000000000000".U)

      while (!c.io.ready.peek().litToBoolean) {
        c.clock.step(1)
      }
      c.clock.step(1)

      c.io.out.sign.expect(1.U)  // Result should be negative
      c.io.out.exp.expect("b01111111".U) // Adjusted exponent should be higher after normalization
      c.io.out.significand.expect("b00000000000000000000000".U)
      c.io.ready.expect(true.B)
      c.clock.step(1)
      c.io.ack.poke(true.B)
      c.clock.step(1)
      c.io.ack.poke(false.B)
    }
  }

  //测试正数和负数除法的结果的符号，以验证符号处理是否正确
  it should "correctly handle the division of negative by positive numbers resulting in a negative number" in {
        test(new FDiv) { c =>
            c.io.valid.poke(true.B)
            c.io.a.sign.poke(1.U)  // negative number
            c.io.a.exp.poke("b01111110".U) // exponent = 126
            c.io.a.significand.poke("b10000000000000000000000".U) // significand

            c.io.b.sign.poke(0.U)  // positive number
            c.io.b.exp.poke("b01111110".U) // exponent = 126
            c.io.b.significand.poke("b10000000000000000000000".U) // significand

            while (!c.io.ready.peek().litToBoolean) {
            c.clock.step(1)
            }
            c.clock.step(1)

            c.io.out.sign.expect(1.U)  // Result should be negative
            c.io.out.exp.expect("b01111111".U) // Adjusted exponent
            c.io.out.significand.expect("b00000000000000000000000".U) // Expected significand
            c.io.ready.expect(true.B)
            c.clock.step(1)
            c.io.ack.poke(true.B)
            c.clock.step(1)
            c.io.ack.poke(false.B)
        }
        }
        //验证在不同尾数值的情况下除法的是否满足预期。
    it should "correctly divide two floating-point numbers with non-trivial significands case1" in {
        test(new FDiv) { c =>
            c.io.valid.poke(true.B)
            c.io.a.sign.poke(0.U)
            c.io.a.exp.poke("b10000000".U)
            c.io.a.significand.poke("b00011110000000000000000".U)  // 10.001111 

            c.io.b.sign.poke(0.U)
            c.io.b.exp.poke("b01111111".U)
            c.io.b.significand.poke("b10100000000000000000000".U)  //1.101

            while (!c.io.ready.peek().litToBoolean) {
            c.clock.step(1)
            }
            c.clock.step(1)

            c.io.out.sign.expect(0.U)  // Result should be positive
            c.io.out.exp.expect("b01111111".U) // Check for expected exponent after division
             c.io.out.significand.expect("b01100000000000000000000".U)  //1.011
            c.io.ready.expect(true.B)
            c.clock.step(1)
            c.io.ack.poke(true.B)
            c.clock.step(1)
            c.io.ack.poke(false.B)
        }
        }

          it should "correctly divide two floating-point numbers with non-trivial significands case2" in {
        test(new FDiv) { c =>
            c.io.valid.poke(true.B)
            c.io.a.sign.poke(0.U)
            c.io.a.exp.poke("b10000000".U)
            c.io.a.significand.poke("b00011110000000000000000".U)  // 10.001111 

            c.io.b.sign.poke(0.U)
            c.io.b.exp.poke("b01111111".U)
            c.io.b.significand.poke("b01100000000000000000000".U)  //1.011

            while (!c.io.ready.peek().litToBoolean) {
            c.clock.step(1)
            }
            c.clock.step(1)

            c.io.out.sign.expect(0.U)  // Result should be positive
            c.io.out.exp.expect("b01111111".U) // Check for expected exponent after division
             c.io.out.significand.expect("b10100000000000000000000".U)  //1.101
            c.io.ready.expect(true.B)
            c.clock.step(1)
            c.io.ack.poke(true.B)
            c.clock.step(1)
            c.io.ack.poke(false.B)
        }
        }


}

package stack_overflow

import org.specs2.mutable.Specification

import scala.reflect.ClassTag

class GenericPrimitiveArraySpec extends Specification {

  /*
    http://stackoverflow.com/questions/35280903/using-generic-primative-arrays-in-scala

    I recently asked this question about whether it was possible to parameterize the type
    of a primative array in Java. (Basically, I have an array that I want to be either a double[]
    or a float[] based on some argument. I'm using arrays of primatives rather than arrays of
    wrapper classes (Double[], Float[]) because they're much more memory-and-time efficient,
    especially when dealing with big vector operations. The answer, it appears, is No, you can't do that in Java.

    So, my new question is: Can I do this in Scala? I understand that primatives are kind of more
    hidden, and only boxed on certain operations... So if it is possible to parameterize my arrays
    as being of Double or Float, and the compiler does implement them as primitives, how do I tell if
    they're being unboxed, or otherwise implemented less efficiently than if I were to just go "Find: Double,
    Replace: Float" in my source code?
   */

  class Container[@specialized(Int) T](value: T) {
    def apply(): T = value
  }

  "specialised" should {
    "create a method that's still valid" in {
      new Container(1).apply() must be_==(1)
    }
  }

  "SpecialisedArray" should {
    "work with float" in {
      val array: Array[Float] = SpecialisedArray[Float](1)
      array.update(0, 2.0f)

      array(0) must beTypedEqualTo(2.0f)
    }

    "and double" in {
      val array: Array[Double] = SpecialisedArray[Double](1)
      array.update(0, 2.0d)

      array(0) must beTypedEqualTo(2.0d)
    }
  }


  object SpecialisedArray {
    def apply[@specialized(Float, Double) T: ClassTag](size: Int) = new Array[T](size)
  }
}

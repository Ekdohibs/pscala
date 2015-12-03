class T[A](a : A) {
   def m[A](x : A) : A = x
}
class U() {
   val x = new T[Int](0);
   val y : Int = x.m[Int](false)
}
object Main {
    def main(args: Array[String]) { }
}

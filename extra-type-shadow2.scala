
class T[Int]() {
   def m(x : Int) : Int = x
}
class U() {
   val a = new T[Int]();
   val b = new T[T[Int]];
   val c : Int = b.m(a);
}
object Main {
    def main(args: Array[String]) { }
}


class T[Unit]() {
   def m(x : Unit) : Unit = x
}
class U() {
   val a = new T[Int]();
   val b : Int = a.m(0)
}
object Main {
    def main(args: Array[String]) { }
}

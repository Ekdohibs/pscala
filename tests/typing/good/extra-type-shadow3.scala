
class T[Unit]() {
   def m(x : Unit) {}
}
class U() {
   val a = new T[Int]();
   val b : Unit = a.m(0)
}
object Main {
    def main(args: Array[String]) { }
}

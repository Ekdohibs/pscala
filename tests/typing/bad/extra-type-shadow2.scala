
class T[Int]() {
   def m(x : Int) : Int = x
}
class U() {
   val a = new T[Unit]();
   val b : Int = a.m(())
}
object Main {
    def main(args: Array[String]) { }
}

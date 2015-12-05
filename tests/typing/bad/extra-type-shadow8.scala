
class T[Unit]() {
   def m() : Unit = {}
}
class U() {
   val a = new T[Int]();
   val b : Int = a.m()
}
object Main {
    def main(args: Array[String]) { }
}

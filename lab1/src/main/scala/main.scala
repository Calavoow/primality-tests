import distributed.core.{Messageable, Message, Component}
import java.rmi.registry.LocateRegistry
import java.rmi.server.UnicastRemoteObject
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import distributed.core.Package

object Main{

  //It may be necessary to register a security manager if we go truly distributed.
  //    if(System.getSecurityManager() == null){
  //      println("Setting new security manager")
  //      System.setSecurityManager(new RMISecurityManager())
  //    }

  val port = 1099
  val reg = LocateRegistry.createRegistry(port)

  // provide a thread pool for the futures
  val pool = Executors.newFixedThreadPool(10)
  implicit val exec_context = ExecutionContext.fromExecutor(pool)

  // create the nodes
  val ar = Component.create("localhost", port, "ar")
  val ir = Component.create("localhost", port, "ir")

  def subscribeToNode(n: Component) {
    def componentReceived(n: Component)(pkg: Package) {
      Console.println(s"${Console.GREEN}>> ${n.me.name} ${Console.BLUE} received: $pkg ${Console.RESET} at ${System.nanoTime() / 1000000}")
    }
    def componentError(n: Component)(e: Throwable) {
      Console.println( "Error" )
      clean(n)
    }

    n.inbox.subscribe(
      componentReceived(n) _,
      componentError(n) _,
      {() =>
        clean(n)
        println("DONE")
      }
    )
  }

  def clean(n: UnicastRemoteObject) {
    UnicastRemoteObject.unexportObject(n, true)
  }

  def main(args: Array[String]){
    subscribeToNode(ir)
    subscribeToNode(ar)

    ar.connect(ir)
    ar.connect(ar)
    ir.connect(ar)
    ir.connect(ir)

    List(ar, ir).foreach{m: Messageable =>
      ar.addresses.put(m.me, m)
      ir.addresses.put(m.me, m)
      ar.connect(m)
      ir.connect(m)
    }

    // clean up the global state after both nodes are finished
//    (ar.inbox merge ir.inbox).doOnCompleted({ () => println( "Wooptiedoo" )}).subscribe(
//      { m:Message => ; },
//      { e:Throwable => ; },
//      { () => pool.shutdown() }
//    )

    ar.requestGrant()
    ir.requestGrant()
  }
}

import java.rmi.registry.LocateRegistry
import java.rmi.{RMISecurityManager, RemoteException}
import java.rmi.server.UnicastRemoteObject

println("Hello World")

val port = 1099
//It may be necessary to register a security manager if we go truly distributed.
//    if(System.getSecurityManager() == null){
//      println("Setting new security manager")
//      System.setSecurityManager(new RMISecurityManager())
//    }

val reg = LocateRegistry.createRegistry(port)

val ar = Node.create("localhost", port, "ar")
val ir = Node.create("localhost", port, "ir")

try{
  ar.connect(ir)
  ir.connect(ar)

  object HELLO extends Message
  ar.broadcast(HELLO)
} finally {
  //Unexport the objects, so that the program terminates.
  UnicastRemoteObject.unexportObject(ar, true)
  UnicastRemoteObject.unexportObject(ir, true)

  UnicastRemoteObject.unexportObject(reg, true)
}


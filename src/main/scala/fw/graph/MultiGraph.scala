package fw.graph

object MultiGraph extends App {

  case class Node[A](p: A) {
    var adjacentGroupMap: scala.collection.mutable.Map[String, Seq[Node[A]]] = scala.collection.mutable.Map.empty
    def traverse(g: String, doSomething: Node[A] => Unit): Unit = {
      adjacentGroupMap.getOrElse(g, List.empty).foreach(_.traverse(g, doSomething))
      doSomething(this)
    }
    def copy[B](g: String, map: Node[A] => Node[B]): Node[B] = {
      val node = map(this)
      node.adjacentGroupMap(g) = adjacentGroupMap.getOrElse(g, List.empty).map(_.copy(g, map))
      node
    }
  }

  def buildGraph(refMap: Map[String, List[String]]) = {
    val nodeSet = refMap.map { f => f._1 :: f._2 }.flatten.toSet
    val nodeMap = nodeSet.map { x => x -> Node(x) }.toMap
    refMap.foreach(f => nodeMap(f._1).adjacentGroupMap("successors") = f._2.map { nodeMap(_) })
    nodeMap
  }

  def printNode[A](node: Node[A]) = println(node.p)

  def createNode[A](node: Node[A]) = Node("copy " + node.p)

  val nodeMap = buildGraph(Map("a" -> List("b", "c"), "b" -> List("d", "e"), "c" -> List("e", "f", "b")))

  nodeMap("a").copy("successors", createNode).traverse("", printNode)
  nodeMap("a").traverse("successors", printNode)
}
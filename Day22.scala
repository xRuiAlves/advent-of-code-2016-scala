//> using scala "3.3.0"
//> using jvm "temurin:17"
//> using file util/ResourceUtils.scala
//> using resourceDir inputs

import util.ResourceUtils.readResourceLines

import scala.annotation.tailrec
import scala.collection.mutable

object Day22 {
  def main(args: Array[String]): Unit = {
    val input = readResourceLines("day22.txt")
    val nodes = parseNodes(input)

    println(nodes.length)

    val part1 = countViableNodePairs(nodes)
    val part2 = 0

    println(s"Part 1: $part1")
    println(s"Part 2: $part2")
  }

  case class Coord(x: Int, y: Int)

  object Coord {
    def fromStr(coordStr: String): Coord = coordStr match {
      case s"/dev/grid/node-x$x-y$y" => Coord(x.toInt, y.toInt)
    }
  }

  case class Node(coord: Coord, used: Int, size: Int) {
    lazy final val available: Int = size - used
    lazy final val isEmpty: Boolean = used == 0
  }

  object Node {
    def fromArray(arr: Array[String]): Node = arr match
      case Array(fs, used, size) => Node(
        Coord.fromStr(fs),
        parseSpace(size),
        parseSpace(used)
      )

    def parseSpace(spaceStr: String): Int = spaceStr.dropRight(1).toInt
  }

  def parseNodes(input: Array[String]): Array[Node] = input.drop(2)
    .map(_.split(" ").filter(_.nonEmpty).dropRight(2))
    .map(Node.fromArray)

  def countViableNodePairs(nodes: Array[Node]): Int = nodes
    .flatMap(nodeA => nodes.map(nodeB => (nodeA, nodeB)))
    .count {
      case (nodeA, nodeB) => nodeA != nodeB && !nodeA.isEmpty && nodeB.available >= nodeA.used
    }
}

package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


  trait Level0 extends SolutionChecker {
    /* terrain for level 0*/

    val level =
      """SooT""".stripMargin

    val optsolution = List(Right, Right)
  }

	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("isStanding") {
    new Level1 {
      assert(Block(Pos(0,0), Pos(0,0)).isStanding, "0,0 0,0 should stand")
      assert(!Block(Pos(0,0), Pos(1,0)).isStanding, "0,0 1,0 should lay down")
    }
  }

  test("isLegal") {
    new Level1 {
      assert(Block(Pos(0,0), Pos(0,0)).isLegal, "0,0 0,0 stands on 0,0")
      assert(Block(Pos(0,0), Pos(1,0)).isLegal, "0,0 1,0 lays down")
      assert(!Block(Pos(3,0), Pos(3,0)).isLegal, "3,0 3,0 stands out row")
      assert(!Block(Pos(2,0), Pos(3,0)).isLegal, "2,0 3,0 lays half out row")
      assert(!Block(Pos(1,6), Pos(1,6)).isLegal, "1,6 1,6 stands out col")
      assert(!Block(Pos(1,5), Pos(1,6)).isLegal, "1,5 1,6 lays half out col")
    }
  }

  test("startBlock") {
    new Level1 {
      assert(startBlock === Block(Pos(1,1), Pos(1,1)))
    }
  }

  test("neighbors") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).neighbors.size === 4)
      assert(Block(Pos(1,0), Pos(1,1)).neighbors.size === 4)
    }
  }

  test("legalNeighbors") {
    new Level1 {
      assert(Block(Pos(2,2), Pos(2,2)).legalNeighbors.size === 3, s"can not go down from 2,2.")
      assert(Block(Pos(1,0), Pos(1,1)).legalNeighbors.size === 3, " left is out of bounds")
      assert(Block(Pos(1,5), Pos(2,5)).legalNeighbors.size === 2, "up and right are illegal locations")
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      assert(
        neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet ===
          Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          )
      )
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      assert(
        newNeighborsOnly(
          Set(
            (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).toStream,

          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
        ) === Set(
            (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))
          ).toStream
      )
    }
  }

  test("from level0") {
    new Level0 {
      assert(from(
        Set(
          (startBlock, List.empty)
        ).toStream,
        Set.empty
      ) === Set(
        (Block(Pos(0,0),Pos(0,0)),List()),
        (Block(Pos(0,1),Pos(0,2)),List(Right)),
        (Block(Pos(0,3),Pos(0,3)),List(Right, Right))
      ).toStream

      )
    }
  }

  test("optimal solution for level 1") {
    new Level0 {
      assert(solve(solution) === Block(goal, goal))
    }
  }


	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

}

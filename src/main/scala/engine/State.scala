package engine

import scala.collection.immutable.{HashSet, HashMap}
import scala.util.Random
import scala.util.boundary, boundary.break

/* 
  Class that represents the game state.
  See "State.xlsx" for more explanations.
*/
case class State(player: Boolean, turn: Boolean, private val white: HashSet[Int], private val black: HashSet[Int]) {

  /* 
    player - color of pieces that human has (True - human has white pieces, False - human has black pieces)
    turn - side to move (True - white to move, False - black to move)
    white, black - indices of squares with knights
  */

  private def mapIndex(i: Int): Int = {
    if (player)
      i
    else
      80 - i
  }

  def getWhite: HashSet[Int] = {
    white.map(s => mapIndex(s))
  }

  def getBlack: HashSet[Int] = {
    black.map(s => mapIndex(s))
  }

  def nextStates(): Vector[State] = {

    def helper(first: HashSet[Int], second: HashSet[Int]): Vector[(HashSet[Int], HashSet[Int])] = {
      
      val firstAttackers = first.foldLeft(0)((acc, s) => if (State.attackingSquares.contains(s)) acc + 1 else acc)
      
      val secondAttackers = second.foldLeft(0)((acc, s) => if (State.attackingSquares.contains(s)) acc + 1 else acc)
      
      val canMoveAttackers = 
        firstAttackers > secondAttackers || 
        white.contains(40) || 
        black.contains(40) || 
        firstAttackers == first.size || 
        secondAttackers == second.size
      
      val shouldMoveAttackers = 
        (white.contains(40) || black.contains(40)) && first.filter(s => State.attackingSquares.contains(s)).size > 0

      first.map(initialSquare =>
        if (State.attackingSquares.contains(initialSquare) && !canMoveAttackers)
          Vector()
        else
          val nextSquares = State.moveTable(initialSquare).foldLeft(Vector[Int]())((acc, nextSquare) =>
            if (shouldMoveAttackers && nextSquare != 40)
              acc
            else if (!first.contains(nextSquare))
              acc :+ nextSquare
            else
              acc
          )
          nextSquares.map(s =>
            if (second.contains(s))
              (first - initialSquare + s, second - s)
            else
              (first - initialSquare + s, second)
          )
      )
      .flatten
      .toVector
    }

    if (turn)
      helper(white, black)
        .map(t => State(player, !turn, t._1, t._2))
        .map(s => (s, s.evaluation))
        .sortWith(_._2 > _._2) // move ordering: https://www.chessprogramming.org/Move_Ordering
        .unzip
        ._1
    else
      helper(black, white)
        .map(t => State(player, !turn, t._2, t._1))
        .map(s => (s, s.evaluation))
        .sortWith(_._2 < _._2) // move ordering: https://www.chessprogramming.org/Move_Ordering
        .unzip
        ._1
  }

  private def randomMove(): State = {
    val states = nextStates()
    states(Random.nextInt(states.length))
  }

  def isTerminal: Boolean = {
    (turn && white.contains(40)) || (!turn && black.contains(40)) || white.size == 0 || black.size == 0
  }

  def canMovePiece(s: Int): Boolean = {
    (turn && white.contains(mapIndex(s)))
    ||
    (!turn && black.contains(mapIndex(s)))
  }

  def legalMove(from: Int, to: Int): Boolean = {
    
    val f = mapIndex(from)
    val t = mapIndex(to)
    
    (
      (turn && white.contains(f) && !white.contains(t)) 
      || 
      (!turn && black.contains(f) && !black.contains(t))
    ) 
    && State.moveTable.contains(f) && State.moveTable(f).contains(t)
  }

  def makeMove(from: Int, to: Int): Option[State] = {

    val f = mapIndex(from)
    val t = mapIndex(to)

    if (legalMove(from, to))
      if (turn)
        Some(State(player, !turn, white - f + t, if (black.contains(t)) black - t else black))
      else
        Some(State(player, !turn, if (white.contains(t)) white - t else white, black - f + t))
    else
      None
  }

  /*
    Evaluation function
    https://www.chessprogramming.org/Evaluation
  */
  def evaluation: Int = {
    if ((turn && white.contains(40)) || black.size == 0)
      Integer.MAX_VALUE
    else if (!turn && black.contains(40) || white.size == 0)
      Integer.MIN_VALUE
    else
      val whiteScore = white.map(s => State.pieceSquareTable(s)).sum
      val blackScore = black.map(s => State.pieceSquareTable(s)).sum
      whiteScore - blackScore + white.size * 3000 - black.size * 3000
  }

  /*
    Alpha-Beta pruning algorithm
    https://en.wikipedia.org/wiki/Alpha%E2%80%93beta_pruning
    https://www.chessprogramming.org/Alpha-Beta
  */
  def alphaBeta(timeToThink: Long): State = {

    val startTime = System.nanoTime()

    def visitStates(states: Vector[State], depth: Int, alpha: Int, beta: Int): Option[(List[State], Int)] = {
      boundary:
        states.foldLeft((None: Option[(List[State], Int)], alpha, beta))((acc, state) =>
          val (bestStateOption, alpha, beta) = acc
          
          ab(state, depth - 1, alpha, beta) match {
            case Some(t) if !state.turn => 
              val newState = (state :: t._1, t._2)
              val bestState = bestStateOption.fold(newState)(v => List(v, newState).maxBy(_._2))
              val a = math.max(alpha, bestState._2)
              if (bestState._2 >= beta)
                break(Some(bestState))
              else
                (Some(bestState), a, beta)
            case Some(t) =>
              val newState = (state :: t._1, t._2)
              val bestState = bestStateOption.fold(newState)(v => List(v, newState).minBy(_._2))
              val b = math.min(beta, bestState._2)
              if (bestState._2 <= alpha)
                break(Some(bestState))
              else
                (Some(bestState), alpha, b)
            case None => 
              break(None)
          }

        )
        ._1
    }

    def ab(state: State, depth: Int, alpha: Int, beta: Int): Option[(List[State], Int)] = {
      lazy val childStates = state.nextStates()

      if (System.nanoTime() - startTime > timeToThink)
        None
      else if (state.isTerminal || depth == 0 || childStates.isEmpty)
        Some((List(), state.evaluation))
      else
        visitStates(childStates, depth, alpha, beta)
    }

    lazy val randomState = randomMove()

    /*
      Iterative deepening from depth 1 to 30
      https://www.chessprogramming.org/Iterative_Deepening
    */
    LazyList
      .from((1 to 30).map(depth => ab(this, depth, Integer.MIN_VALUE, Integer.MAX_VALUE)))
      .findLast(o => o.isDefined)
      .getOrElse(Some((List(randomState), randomState.evaluation)))
      .get
      ._1
      .headOption
      .getOrElse(randomState)
  }

}

object State {

  /*
    Piece-square table used in state evaluation
    https://www.chessprogramming.org/Piece-Square_Tables
  */
  val pieceSquareTable = Vector(
    0, 0, 1000, 0, 1000, 0, 1000, 0, 0,
    0, 1000, 0, 1000, 0, 1000, 0, 1000, 0,
    1000, 0, 0, 10000, 1000, 10000, 0, 0, 1000,
    0, 1000, 30000, 1000, 0, 1000, 30000, 1000, 0,
    1000, 0, 1000, 0, 999999, 0, 1000, 0, 1000,
    0, 1000, 30000, 1000, 0, 1000, 30000, 1000, 0,
    1000, 0, 0, 10000, 1000, 10000, 0, 0, 1000,
    0, 1000, 0, 1000, 0, 1000, 0, 1000, 0,
    0, 0, 1000, 0, 1000, 0, 1000, 0, 0
  )

  // Squares that attack central square (40)
  val attackingSquares = HashSet(23, 33, 51, 59, 57, 47, 29, 21)

  /*
    Table with pre-calculated moves
    https://www.chessprogramming.org/Table-driven_Move_Generation
  */
  val moveTable = generateMoveTable()

  def initialState(player: Boolean): State = {
    State(player, true, HashSet.from(72 to 80), HashSet.from(0 to 8))
  }

  /*
    Creates the table with pre-calculated moves
    https://www.chessprogramming.org/Table-driven_Move_Generation
  */
  private def generateMoveTable(): HashMap[Int, HashSet[Int]] = {

    def reduce(i: Int): Int = {
      i / 18 * 9 + (i % 9)
    }

    val offsets = HashSet(-35, -16, 20, 37, 35, 16, -20, -37)

    val free = HashSet.from(
      (0 to 16 by 2).map(i =>
        (0 until 9).map(j =>
          9 * i + j
        )
      )
      .flatten
    )

    val forbidden = HashSet.from(
      (1 to 17 by 2).map(i =>
        (0 until 9).map(j =>
          9 * i + j
        )
      )
      .flatten
    )

    def legalSquare(square: Int): Boolean = {
      !forbidden.contains(square) && square >= 0 && square < 153
    }

    HashMap.from(
      free.map(square =>
        (
          reduce(square),
          offsets
            .foldLeft(HashSet[Int]())((acc, offset) =>
              val newSquare = square + offset
              if (legalSquare(newSquare))
                acc + newSquare
              else
                acc
            )
            .map(i => reduce(i))
        )
      )
    )
  }

  def moveTableToString(): String = {
    moveTable.toVector.sortWith(_._1 < _._1).mkString("\n")
  }

}

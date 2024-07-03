package gui.listeners

import java.awt.event.MouseEvent
import java.awt.event.MouseAdapter
import gui.Board
import javax.swing.JLabel
import javax.swing.JPanel
import javax.swing.JLayeredPane
import gui.EngineSwingWorker
import javax.swing.JOptionPane
import javax.swing.SwingUtilities
import scala.collection.immutable.HashSet

class KnightListener(board: Board, squareSize: Int) extends MouseAdapter {

  var piece: Option[JLabel] = None
  var from = 0

  def xyToI(x: Int, y: Int): Int = {
    y / squareSize * 9 + x / squareSize
  }

  override def mousePressed(e: MouseEvent): Unit = {
    piece = None
    from = xyToI(e.getX(), e.getY())
    val p = board.squares.getComponent(from).asInstanceOf[JPanel]

    if (!board.state.isTerminal && board.state.player == board.state.turn && board.state.canMovePiece(from) && p.getComponentCount() == 1)
      board.paintSquares(from)
      piece = Some(p.getComponent(0).asInstanceOf[JLabel])
      piece.get.setLocation(e.getX() - piece.get.getWidth() / 2, e.getY() - piece.get.getHeight() / 2)
      board.layeredPane.add(piece.get, JLayeredPane.DRAG_LAYER)
  }

  override def mouseDragged(e: MouseEvent): Unit = {
    piece.map(l => l.setLocation(e.getX() - squareSize / 2, e.getY() - squareSize / 2))
  }

  override def mouseReleased(e: MouseEvent): Unit = {
    piece.map(l => 
      val to = xyToI(e.getX(), e.getY())
      if (
          e.getX() < 0 
          || e.getX() >= squareSize * 9 
          || e.getY() < 0 
          || e.getY() >= squareSize * 9 
          || !board.state.legalMove(from, to)
      )
        placePiece(from)
      else
        placePiece(to)
    )
  }

  private def placePiece(to: Int): Unit = {
    board.paintBoard()
    piece.get.setVisible(false)
    val panel = board.squares.getComponent(to).asInstanceOf[JPanel]
    panel.removeAll()
    panel.add(piece.get)
    piece.get.setVisible(true)
    board.state
      .makeMove(from, to)
      .map(state =>
        board.state = state
        board.lastMoveSquares = Some(HashSet(from, to))
        board.paintBoard()
        board.computerMove()
      )
  }
}

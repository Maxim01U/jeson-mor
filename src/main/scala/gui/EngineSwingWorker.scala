package gui

import javax.swing.SwingWorker
import engine.State
import javax.swing.JOptionPane

class EngineSwingWorker(board: Board, time: Long) extends SwingWorker[State, Unit] {

  override def doInBackground(): State = {
    board.state.alphaBeta(time)
  }

  override def done(): Unit = {
    if (!isCancelled())
      val state = get()
      board.arrangeState(state, true)
      board.showMessage.map(message =>
        JOptionPane.showMessageDialog(null, message)
      )
  }

}

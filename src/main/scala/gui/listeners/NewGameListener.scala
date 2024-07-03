package gui.listeners

import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import gui.MainFrame
import engine.State

class NewGameListener(mainFrame: MainFrame) extends ActionListener {
  override def actionPerformed(e: ActionEvent): Unit = {
    mainFrame.board.worker.map(sw =>
      sw.cancel(true)
    )
    mainFrame.board.arrangeState(State.initialState(!mainFrame.board.state.player), false)
    mainFrame.board.computerMove()
  }
}

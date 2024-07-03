package gui

import javax.swing.JFrame
import javax.swing.JPanel
import engine.State
import javax.swing.JMenuBar
import javax.swing.JMenu
import javax.swing.JMenuItem
import gui.listeners.NewGameListener

class MainFrame() extends JFrame {
  setTitle("Jeson Mor (Nine Knights)")
  val mainPanel = JPanel()

  val board = Board(State.initialState(true))

  mainPanel.add(board)

  val bar = JMenuBar()
  val game = JMenu("Game")
  val newGame = JMenuItem("New")
  game.add(newGame)
  bar.add(game)
  newGame.addActionListener(NewGameListener(this))
  setJMenuBar(bar)

  getContentPane().add(mainPanel)
  pack()
  setVisible(true)
}

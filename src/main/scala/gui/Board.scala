package gui

import javax.swing.JPanel
import engine.State
import javax.swing.ImageIcon
import java.awt.Color
import java.awt.Dimension
import java.awt.GridLayout
import javax.swing.JLayeredPane
import java.awt.BorderLayout
import javax.swing.JLabel
import gui.listeners.KnightListener
import java.awt.image.BufferedImage
import java.awt.RenderingHints
import javax.swing.JOptionPane
import javax.swing.JFrame
import java.awt.GridBagLayout
import java.awt.GridBagConstraints
import java.awt.Insets
import javax.swing.JSlider
import scala.jdk.CollectionConverters.*
import scala.collection.immutable.HashSet

class Board(var state: State) extends JPanel {

  var worker: Option[EngineSwingWorker] = None

  var lastMoveSquares: Option[HashSet[Int]] = None

  val squareSize = 75

  val dimension = Dimension(squareSize * 9, squareSize * 9)

  val listener = KnightListener(this, squareSize)

  val squares = JPanel()
  squares.setLayout(GridLayout(9, 9))
  squares.setPreferredSize(dimension)
  squares.setBounds(0, 0, dimension.width, dimension.height)
  squares.addMouseListener(listener)
  squares.addMouseMotionListener(listener)

  val layeredPane = JLayeredPane()
  layeredPane.setPreferredSize(dimension)
  layeredPane.add(squares, JLayeredPane.DEFAULT_LAYER)

  (0 to 80).foreach(i => squares.add(JPanel(BorderLayout())))

  val camel = Color(193, 154, 107)
  val bisque = Color(255, 228, 196)
  val darkKhaki = Color(189, 183, 107)
  val mocha = Color(150, 121, 105)
  val lightSteelBlue = Color(176, 196, 222)

  val slider = JSlider(1, 20, 4)
  slider.setLabelTable(scala.collection.mutable.HashMap.from((1 to 20).map(i => (i, JLabel((i / 2.0).toString())))).asJavaDictionary)
  slider.setMajorTickSpacing(1)
  slider.setPaintLabels(true)
  slider.setPaintTicks(true)
  slider.setPaintTrack(true)

  val label = JLabel("time per move (s):")

  setLayout(GridBagLayout())
  val gbc = GridBagConstraints()
  gbc.fill = GridBagConstraints.BOTH
  gbc.gridwidth = 2
  add(layeredPane, gbc)
  gbc.gridy = 1
  gbc.gridwidth = 1
  gbc.insets = Insets(0, 30, 10, 0)
  add(label, gbc)
  gbc.gridx = 1
  gbc.fill = GridBagConstraints.HORIZONTAL
  gbc.insets = Insets(0, 10, 10, 30)
  add(slider, gbc)

  arrangeState(state, false)

  private def getKnightLabel(path: String): JLabel = {
    val imageIcom = ImageIcon(path)
    val image = imageIcom.getImage()
    val scaledImage = image.getScaledInstance(75, 75,  java.awt.Image.SCALE_SMOOTH)
    val scaledImageIcon = ImageIcon(scaledImage)
    JLabel(scaledImageIcon)
  }

  def paintBoard(): Unit = {
    (0 to 80).foreach(i => 
      squares.getComponent(i).setBackground(if (i % 2 == 0) camel else bisque)
    )
    squares.getComponent(40).setBackground(mocha)
    lastMoveSquares.map(set => set.foreach(s => paintSquare(s, lightSteelBlue)))
  }

  def paintSquare(s: Int, color: Color): Unit = {
    squares.getComponent(s).setBackground(color)
  }

  def paintSquares(s: Int): Unit = {
    State.moveTable(s).foreach(i => 
      if ((state.player && !state.getWhite.contains(i)) || (!state.player && !state.getBlack.contains(i)))
        paintSquare(i, darkKhaki)
    )
  }

  def arrangeState(other: State, lights: Boolean): Unit = {
    if (lights)
      lastMoveSquares = Some(
        state.getWhite.diff(other.getWhite) 
        ++ state.getBlack.diff(other.getBlack) 
        ++ other.getWhite.diff(state.getWhite) 
        ++ other.getBlack.diff(state.getBlack)
      )
    else
      lastMoveSquares = None
    
    paintBoard()

    this.state = other
    (0 to 80).foreach(i =>
      squares.getComponent(i).asInstanceOf[JPanel].removeAll()
    )

    state.getWhite.foreach(i =>
      squares.getComponent(i).asInstanceOf[JPanel].add(getKnightLabel("textures/white-knight.png"))
    )
    state.getBlack.foreach(i =>
      squares.getComponent(i).asInstanceOf[JPanel].add(getKnightLabel("textures/black-knight.png"))
    )

    squares.revalidate()
    squares.repaint()
  }
  
  def showMessage: Option[String] = {
    if (
      state.isTerminal 
      && (
        (state.evaluation == Integer.MAX_VALUE && state.player) 
        || (state.evaluation == Integer.MIN_VALUE && !state.player)
      )
    )
      Some("You won!")
    else if (state.isTerminal)
      Some("You lost!")
    else 
      None
  }

  def computerMove(): Unit = {
    if (state.player != state.turn)
      showMessage match {
        case Some(value) => 
          JOptionPane.showMessageDialog(null, value)
        case None => 
          worker = Some(EngineSwingWorker(this, (slider.getValue() / 2.0 * 1000000000l).toLong))
          worker.get.execute()
      }
  }

}

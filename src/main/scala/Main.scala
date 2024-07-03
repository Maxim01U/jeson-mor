import gui.MainFrame
import javax.swing.SwingUtilities
import com.formdev.flatlaf.FlatLightLaf

@main def main(): Unit =
  SwingUtilities.invokeLater(() =>
    FlatLightLaf.setup()
    MainFrame()
  )

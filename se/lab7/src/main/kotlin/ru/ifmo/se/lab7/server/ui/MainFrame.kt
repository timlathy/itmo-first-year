package ru.ifmo.se.lab7.server.ui

import javax.swing.JFrame
import javax.swing.JTabbedPane
import javax.swing.JTree

class MainFrame(private val user: String): JFrame() {
  init {
    setSize(600, 800)
    setLocationRelativeTo(null)

    title = "Employment Request Server [logged in as $user]"
    isVisible = true
    defaultCloseOperation = JFrame.EXIT_ON_CLOSE

    contentPane.add(JTabbedPane().apply {
      val objectTree = JTree()

      addTab("Object View", objectTree)
    })
  }
}

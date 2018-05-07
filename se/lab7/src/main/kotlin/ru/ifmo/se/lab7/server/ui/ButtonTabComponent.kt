package ru.ifmo.se.lab7.server.ui

import javax.swing.AbstractButton
import java.awt.Color
import java.awt.BasicStroke
import java.awt.Graphics2D
import java.awt.Graphics
import javax.swing.BorderFactory
import javax.swing.plaf.basic.BasicButtonUI
import java.awt.Dimension
import javax.swing.JButton
import javax.swing.JLabel
import java.awt.FlowLayout
import java.awt.event.*
import javax.swing.JTabbedPane
import javax.swing.JPanel

/* Based on https://docs.oracle.com/javase/tutorial/uiswing/components/tabbedpane.html
 * (Copyright (c) 1995, 2008, Oracle and/or its affiliates)  */
class ButtonTabComponent(private val pane: JTabbedPane) : JPanel(FlowLayout(FlowLayout.LEFT, 0, 0)) {
  init {
    isOpaque = false
    border = BorderFactory.createEmptyBorder(4, 0, 2, 0)

    add(object : JLabel() {
      override fun getText(): String? = tabComponentIndex()?.let(pane::getTitleAt)
    }.apply {
      border = BorderFactory.createEmptyBorder(0, 0, 0, 6)
    })
    add(TabButton())
  }

  private fun tabComponentIndex(): Int? =
    pane.indexOfTabComponent(this).takeIf { it != -1 }

  private inner class TabButton : JButton() {
    init {
      val size = 17
      preferredSize = Dimension(size, size)
      toolTipText = "Close this tab"
      setUI(BasicButtonUI())
      isContentAreaFilled = false
      isFocusable = false
      border = BorderFactory.createEtchedBorder()
      isRolloverEnabled = true

      isBorderPainted = false
      addMouseListener(object : MouseAdapter() {
        override fun mouseEntered(e: MouseEvent) = e.component.run {
          if (this is AbstractButton) isBorderPainted = true
        }

        override fun mouseExited(e: MouseEvent)  = e.component.run {
          if (this is AbstractButton) isBorderPainted = false
        }
      })

      addActionListener { tabComponentIndex()?.run(pane::remove) }
    }

    override fun updateUI() { }

    override fun paintComponent(g: Graphics) {
      super.paintComponent(g)

      val crossSize = 6

      with(g.create() as Graphics2D) {
        stroke = BasicStroke(2f)
        color = if (model.isRollover) Color.BLACK else Color(0, 0, 0, 200)

        drawLine(crossSize, crossSize, width - crossSize - 1, height - crossSize - 1)
        drawLine(width - crossSize - 1, crossSize, crossSize, height - crossSize - 1)
        dispose()
      }
    }
  }
}

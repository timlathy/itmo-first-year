package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.CommandRunner
import ru.ifmo.se.lab7.server.EmploymentRequest
import java.awt.Color
import java.time.LocalDateTime
import javax.swing.*
import javax.swing.table.DefaultTableModel
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel
import java.awt.Component
import java.awt.Dimension
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.TableCellRenderer

@JvmField val COLOR_LOG_TABLE_ADDITION_ODD_ROW = Color(0xccffd9)
@JvmField val COLOR_LOG_TABLE_ADDITION_EVEN_ROW = Color(0xe4ffeb)
@JvmField val COLOR_LOG_TABLE_REMOVAL_ODD_ROW = Color(0xffdfd4)
@JvmField val COLOR_LOG_TABLE_REMOVAL_EVEN_ROW = Color(0xffebe4)

class MainFrame(private val user: String,
                private val runner: CommandRunner<EmploymentRequest>): JFrame() {
  private val objectTree = JTree(DefaultMutableTreeNode().apply {
    EmploymentRequest.Status.values().forEach { add(DefaultMutableTreeNode(it)) }
  })
  private val logTableCols = arrayOf("User", "Date", "Command", "Element")
  private val logTable = JTable(LogTableModel(logTableCols)).apply {
    setDefaultRenderer(Object::class.java, LogTableRenderer(elementColIndex = logTableCols.lastIndex))
  }

  init {
    size = Dimension(600, 800)
    minimumSize = Dimension(400, 600)
    setLocationRelativeTo(null)

    title = "Employment Request Server [logged in as $user]"
    isVisible = true
    defaultCloseOperation = JFrame.EXIT_ON_CLOSE

    contentPane.add(JTabbedPane().apply {
      val logPane = JScrollPane(logTable)
      logTable.fillsViewportHeight = true

      addTab("Object View", objectTree)
      addTab("Client Interaction Log", logPane)
    })

    runner.addCommandListener(::onCommandExecuted)
  }

  fun onCommandExecuted(commandName: String, changes: List<CollectionChange<EmploymentRequest>>) {
    val logModel = logTable.model as DefaultTableModel

    changes.forEach {
      logModel.addRow(arrayOf("test", LocalDateTime.now(), commandName, it))
    }

    val viewModel = objectTree.model as DefaultTreeModel
    val viewRoot = viewModel.root as DefaultMutableTreeNode

    changes.forEach { (element, status) ->
      val statusRoot = viewRoot.getChildAt(element.status.ordinal) as DefaultMutableTreeNode
      when (status) {
        CollectionChange.ChangeType.ADDITION ->
          viewModel.insertNodeInto(DefaultMutableTreeNode(element), statusRoot, statusRoot.childCount)
        CollectionChange.ChangeType.REMOVAL ->
          statusRoot.depthFirstEnumeration().iterator().asSequence()
            .find { (it as DefaultMutableTreeNode).userObject == element }
            ?.let { viewModel.removeNodeFromParent(it as DefaultMutableTreeNode) }
      }
    }
  }

  class LogTableModel(cols: Array<String>): DefaultTableModel(cols, 0) {
    override fun isCellEditable(row: Int, column: Int) = false
  }

  class LogTableRenderer(private val elementColIndex: Int): TableCellRenderer, DefaultTableCellRenderer() {
    override fun getTableCellRendererComponent(table: JTable?, value: Any?, isSelected: Boolean, hasFocus: Boolean,
                                               row: Int, column: Int): Component =
      super
        .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        .apply {
          table?.model
            ?.getValueAt(row, elementColIndex)
            ?.let {
              val change = it as CollectionChange<EmploymentRequest>

              if (column == elementColIndex) text = change.element.toString()

              background = when (change.type) {
                CollectionChange.ChangeType.ADDITION ->
                  if (row % 2 == 0) COLOR_LOG_TABLE_ADDITION_EVEN_ROW else COLOR_LOG_TABLE_ADDITION_ODD_ROW
                CollectionChange.ChangeType.REMOVAL ->
                  if (row % 2 == 0) COLOR_LOG_TABLE_REMOVAL_EVEN_ROW else COLOR_LOG_TABLE_REMOVAL_ODD_ROW
              }
            }
        }
  }
}

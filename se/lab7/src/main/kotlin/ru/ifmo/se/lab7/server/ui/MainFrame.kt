package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.CommandRunner
import ru.ifmo.se.lab7.server.EmploymentRequest
import java.time.LocalDateTime
import javax.swing.*
import javax.swing.table.DefaultTableModel
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeModel

class MainFrame(private val user: String,
                private val runner: CommandRunner<EmploymentRequest>): JFrame() {
  private val objectTree: JTree = JTree(DefaultMutableTreeNode().apply {
    EmploymentRequest.Status.values().forEach { add(DefaultMutableTreeNode(it)) }
  })
  private val logTable: JTable = JTable(
    DefaultTableModel(arrayOf("User", "Date", "Command", "Change"), 0))

  init {
    setSize(600, 800)
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
}

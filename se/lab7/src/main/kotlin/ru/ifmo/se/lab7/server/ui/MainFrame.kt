package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.CommandRunner
import ru.ifmo.se.lab7.server.EmploymentRequest
import javax.swing.*
import java.awt.Dimension

class MainFrame(private val user: String,
                private val runner: CommandRunner<EmploymentRequest>): JFrame() {
  private val objectTree = EmploymentRequestTree()
  private val logTable = InteractionLogTable<EmploymentRequest>()

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

    runner.addCommandListener { commandName, changes: List<CollectionChange<EmploymentRequest>> ->
      objectTree.updateWithChanges(changes)
      logTable.insertChanges("test", commandName, changes)
    }
  }
}

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
  private val mainPane = JTabbedPane().apply {
    addTab("Object View", objectTree)
    addTab("Client Interaction Log", JScrollPane(logTable))

    logTable.fillsViewportHeight = true /* placed inside a scroll pane */
  }

  private val buttonAdd = JButton("Add").apply { addActionListener { openEditor() } }
  private val buttonRemove = JButton("Remove")

  init {
    size = Dimension(600, 800)
    minimumSize = Dimension(400, 600)
    setLocationRelativeTo(null)

    title = "Employment Request Server [logged in as $user]"
    isVisible = true
    defaultCloseOperation = JFrame.EXIT_ON_CLOSE

    contentPane.layout = GroupLayout(contentPane).apply {
      autoCreateGaps = true
      autoCreateContainerGaps = true

      setHorizontalGroup(
        createParallelGroup(GroupLayout.Alignment.LEADING)
          .addGroup(createSequentialGroup()
            .addComponent(buttonAdd)
            .addComponent(buttonRemove))
          .addComponent(mainPane)
      )
      setVerticalGroup(
        createSequentialGroup()
          .addGroup(createParallelGroup(GroupLayout.Alignment.BASELINE)
            .addComponent(buttonAdd)
            .addComponent(buttonRemove))
          .addComponent(mainPane)
      )
    }

    runner.addCommandListener { commandName, changes: List<CollectionChange<EmploymentRequest>> ->
      objectTree.updateWithChanges(changes)
      logTable.insertChanges("test", commandName, changes)
    }
  }

  fun openEditor() {
    val editor = EmploymentRequestEditorComponent()
    mainPane.addTab("New Request", editor)
    mainPane.selectedIndex = mainPane.tabCount - 1
    mainPane.setTabComponentAt(mainPane.tabCount - 1, ButtonTabComponent(mainPane))

    editor.addEditingFinishListener(object : EmploymentRequestEditorComponent.EditingFinishEventListener {
      override fun onEditingFinish(e: EmploymentRequestEditorComponent.EditingFinishEvent) {
        mainPane.remove(editor)
        runner.eval("add", e.newObject)
      }
    })
  }
}

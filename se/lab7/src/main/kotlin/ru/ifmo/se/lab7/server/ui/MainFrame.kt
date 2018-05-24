package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import ru.ifmo.se.lab7.server.CommandRunner
import ru.ifmo.se.lab7.server.EmploymentRequest
import javax.swing.*
import java.awt.Dimension
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent

class MainFrame(private val user: String,
                private val runner: CommandRunner<EmploymentRequest>): JFrame() {
  private val treeElement = EmploymentRequestTree()
  private val tableLog = InteractionLogTable<EmploymentRequest>()
  private val paneMain = JTabbedPane().apply {
    addTab("Element View", treeElement)
    addTab("Client Interaction Log", JScrollPane(tableLog))

    tableLog.fillsViewportHeight = true /* placed inside a scroll pane */
  }
  private val fileChooser = JFileChooser()
  private val menu: JMenuBar = JMenuBar().apply {
    add(JMenu("File").apply {
      mnemonic = KeyEvent.VK_F
      add(JMenuItem("Save queue to file...").apply {
        accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_S, ActionEvent.CTRL_MASK)
        addActionListener {
          if (fileChooser.showSaveDialog(this) == JFileChooser.APPROVE_OPTION) {
            val result = runner.saveQueue(fileChooser.selectedFile)
            if (!result) {
              JOptionPane.showMessageDialog(this@MainFrame, null,
                "An error has occurred while saving the queue", JOptionPane.ERROR_MESSAGE)
            }
          }
        }
      })
      add(JMenuItem("Import queue from a file...").apply {
        accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK)
        this.addActionListener {

          if (fileChooser.showOpenDialog(this) == JFileChooser.APPROVE_OPTION) {
            val result = runner.openQueue<EmploymentRequest>(fileChooser.selectedFile)
            if (!result) {
              JOptionPane.showMessageDialog(this@MainFrame, null,
                "An error has occurred while loading the file", JOptionPane.ERROR_MESSAGE)
            }
          }
        }
      })
    })
  }

  private val commandExecutor = object : CollectionControlComponent.UICommandExecutor {
    override fun clear() { runner.eval("clear", null) }

    override fun add(element: EmploymentRequest) { runner.eval("add", element) }

    override fun addIfHighestPriority(element: EmploymentRequest) { runner.eval("add_if_max", element) }

    override fun addIfLowestPriority(element: EmploymentRequest) { runner.eval("add_if_min", element) }

    override fun removeElement(element: EmploymentRequest) { runner.eval("remove", element) }

    override fun removeHigherPriority(element: EmploymentRequest) { runner.eval("remove_greater", element) }

    override fun removeLowerPriority(element: EmploymentRequest) { runner.eval("remove_lower", element) }

    override fun removeHighestPriority() { runner.eval("remove_first", null) }

    override fun removeLowestPriority() { runner.eval("remove_last", null) }

  }

  private val compControl = CollectionControlComponent(paneMain, commandExecutor)

  init {
    size = Dimension(600, 800)
    minimumSize = Dimension(400, 600)
    setLocationRelativeTo(null)

    title = "Employment Request Server [logged in as $user]"
    isVisible = true
    defaultCloseOperation = JFrame.EXIT_ON_CLOSE
    jMenuBar = menu

    treeElement.apply { addElementSelectionChangeListener(compControl) }

    contentPane.layout = GroupLayout(contentPane).apply {
      autoCreateGaps = true
      autoCreateContainerGaps = true

      setHorizontalGroup(
        createParallelGroup(GroupLayout.Alignment.LEADING)
          .addComponent(compControl)
          .addComponent(paneMain)
      )

      setVerticalGroup(
        createSequentialGroup()
          .addComponent(compControl)
          .addComponent(paneMain)
      )

      pack()
    }

    runner.addCommandListener { commandName, changes: List<CollectionChange<EmploymentRequest>> ->
      treeElement.updateWithChanges(changes)
      tableLog.insertChanges("test", commandName, changes)
    }
  }
}

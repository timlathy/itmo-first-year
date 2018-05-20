package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.EmploymentRequest
import ru.ifmo.se.lab7.server.ui.EmploymentRequestTree.ElementSelectionChangeEvent
import ru.ifmo.se.lab7.server.ui.EmploymentRequestTree.ElementSelectionChangeEventListener
import com.alexandriasoftware.swing.JSplitButton
import javax.swing.*

class CollectionControlComponent(private val tabArea: JTabbedPane,
                                 private val executor: UICommandExecutor): JComponent(), ElementSelectionChangeEventListener {
  interface UICommandExecutor {
    fun addElement(element: EmploymentRequest)
    fun removeElement(element: EmploymentRequest)
    fun removeHigherPriority(element: EmploymentRequest)
    fun removeLowerPriority(element: EmploymentRequest)
    fun removeHighestPriority()
    fun removeLowestPriority()
    fun clear()
  }

  private var selectedElement: EmploymentRequest? = null

  override fun onElementSelectionChanged(e: ElementSelectionChangeEvent) {
    selectedElement = e.selected
    updateSelectionDependentControls()
  }

  private fun updateSelectionDependentControls() {
    buttonRemove.isEnabled = (selectedElement != null)
    buttonRemove.toolTipText =
      if (selectedElement == null) "You need to select an element to be removed"
      else ""
  }

  private fun openEditorTab() {
    val editor = EmploymentRequestEditorComponent()
    with(tabArea) {
      addTab("New Request", editor)
      setTabComponentAt(tabCount - 1, ButtonTabComponent(this))
      selectedIndex = tabCount - 1
    }

    editor.addEditingFinishListener(object : EmploymentRequestEditorComponent.EditingFinishEventListener {
      override fun onEditingFinish(e: EmploymentRequestEditorComponent.EditingFinishEvent) {
        tabArea.remove(editor)
        executor.addElement(e.newObject)
      }
    })
  }

  private val buttonAdd = JButton("Add").apply { addActionListener { openEditorTab() } }

  private val buttonRemove = JSplitButton("Remove...").apply {
    popupMenu = JPopupMenu()
      .withCommand("Remove all with higher priority", { selectedElement?.let(executor::removeHigherPriority) })
      .withCommand("Remove all with lower priority", { selectedElement?.let(executor::removeLowerPriority) })

    addButtonClickedActionListener { selectedElement?.let(executor::removeElement) }
  }

  private val buttonClear = JSplitButton("Clear...").apply {
    popupMenu = JPopupMenu()
      .withCommand("Remove highest-priority element", executor::removeHighestPriority)
      .withCommand("Remove lowest-priority element", executor::removeLowestPriority)

    addButtonClickedActionListener { executor.clear() }
  }

  init {
    layout = GroupLayout(this).apply {
      setHorizontalGroup(
        createSequentialGroup()
          .addComponent(buttonClear)
          .addComponent(buttonAdd)
          .addComponent(buttonRemove)
      )

      setVerticalGroup(
        createParallelGroup(GroupLayout.Alignment.LEADING)
          .addComponent(buttonClear)
          .addComponent(buttonAdd)
          .addComponent(buttonRemove)
      )
    }
    updateSelectionDependentControls()
  }

  inline fun JPopupMenu.withCommand(label: String, crossinline command: () -> Unit) = apply {
    add(JMenuItem(label).apply { addActionListener { command() } })
  }
}

package ru.ifmo.se.lab7.server.ui

import ru.ifmo.se.lab7.server.CollectionChange
import java.awt.Color
import java.awt.Component
import java.time.LocalDateTime
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.table.DefaultTableModel
import javax.swing.table.TableCellRenderer

class InteractionLogTable<E>: JTable(LogTableModel(tableColumns)) {
  companion object {
    private val tableColumns = arrayOf("User", "Date", "Command", "Element")
  }

  init {
    setDefaultRenderer(Object::class.java, LogTableRenderer<E>(elementColIndex = tableColumns.lastIndex))
  }

  fun insertChanges(user: String, commandName: String, changes: List<CollectionChange<E>>) =
    changes.forEach { model.addRow(arrayOf(user, LocalDateTime.now(), commandName, it)) }

  override fun getModel(): LogTableModel = super.getModel() as LogTableModel

  class LogTableModel(cols: Array<String>): DefaultTableModel(cols, 0) {
    override fun isCellEditable(row: Int, column: Int) = false
  }

  class LogTableRenderer<E>(private val elementColIndex: Int): TableCellRenderer, DefaultTableCellRenderer() {
    val COLOR_LOG_TABLE_ADDITION_ODD_ROW = Color(0xccffd9)
    val COLOR_LOG_TABLE_ADDITION_EVEN_ROW = Color(0xe4ffeb)
    val COLOR_LOG_TABLE_REMOVAL_ODD_ROW = Color(0xffdfd4)
    val COLOR_LOG_TABLE_REMOVAL_EVEN_ROW = Color(0xffebe4)

    override fun getTableCellRendererComponent(table: JTable?, value: Any?, isSelected: Boolean, hasFocus: Boolean,
                                               row: Int, column: Int): Component =
      super
        .getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        .apply {
          table?.model
            ?.getValueAt(row, elementColIndex)
            ?.let {
              val change = it as CollectionChange<E>

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

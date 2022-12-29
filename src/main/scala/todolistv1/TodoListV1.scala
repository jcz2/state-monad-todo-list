package todolistv1

import java.util.UUID.randomUUID
import scala.collection.mutable

case class Todo(id: String, description: String, completed: Boolean)

class TodoListV1 {
  private val todos = mutable.Map[String, Todo]()

  def add(description: String): Todo = {
    val todo = Todo(randomUUID.toString, description, completed = false)
    todos.addOne(todo.id, todo)
    todo
  }

  def remove(id: String): Unit = todos.remove(id)

  def update(id: String, completed: Boolean): Unit = {
    val todo = todos.get(id)
    if (todo.isDefined) {
      todos.update(id, todo.get.copy(completed = completed))
    }
  }

  def check(id: String): Unit = update(id, completed = true)

  def uncheck(id: String): Unit = update(id, completed = false)

  def isCompleted: Boolean = todos.forall { case (_, todo) => todo.completed }

  def clear(): Unit = todos.clear()
}

object Main extends App {
  val todoList = new TodoListV1()
  val todo = todoList.add("Make lunch")
  todoList.check(todo.id)
  if (todoList.isCompleted) {
    todoList.clear()
  }
}
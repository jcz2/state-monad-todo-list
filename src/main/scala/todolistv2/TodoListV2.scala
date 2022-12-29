package todolistv2

import java.util.UUID.randomUUID

case class Todo(id: String, description: String, completed: Boolean)
case class TodoList(todos: Map[String, Todo] = Map())

object TodoList {
  def add(todoList: TodoList, description: String): (Todo, TodoList) = {
    val todo = Todo(randomUUID.toString, description, completed = false)
    val newTodoList = todoList.copy(todos = todoList.todos.updated(todo.id, todo))
    (todo, newTodoList)
  }

  def remove(todoList: TodoList, id: String): TodoList =
    todoList.copy(todos = todoList.todos.removed(id))

  def check(todoList: TodoList, id: String): TodoList =
    update(todoList, id, completed = true)

  def uncheck(todoList: TodoList, id: String): TodoList =
    update(todoList, id, completed = false)

  def update(todoList: TodoList, id: String, completed: Boolean): TodoList =
    todoList.todos.get(id)
      .map(todo => {
        todoList.copy(todos = todoList.todos.updated(id, todo.copy(completed = completed)))
      })
      .getOrElse(todoList)

  def isCompleted(todoList: TodoList): Boolean =
    todoList.todos.forall { case (_, todo) => todo.completed }

  def clear(todoList: TodoList): TodoList =
    todoList.copy(todos = Map[String, Todo]())
}

object Main extends App {
  val todoList = TodoList()
  val (todo, todoList1) = TodoList.add(todoList, "Make lunch")
  val todoList2 = TodoList.check(todoList1, todo.id)
  val todoList3 = if (TodoList.isCompleted(todoList2)) {
    TodoList.clear(todoList2)
  } else {
    todoList2
  }
}
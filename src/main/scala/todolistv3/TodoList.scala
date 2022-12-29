package todolistv3

import java.util.UUID.randomUUID
import cats.data.State

case class Todo(id: String, description: String, completed: Boolean)
case class TodoList(todos: Map[String, Todo])

object TodoList {
  def add(description: String): State[TodoList, Todo] = for {
    todoList <- State.get[TodoList]
    todo = Todo(randomUUID.toString, description, completed = false)
    _ <- State.set(todoList.copy(todos = todoList.todos.updated(todo.id, todo)))
  } yield todo

  def remove(id: String): State[TodoList, Unit] = State { todoList =>
    (todoList.copy(todos = todoList.todos.removed(id)), ())
  }

  def check(id: String): State[TodoList, Unit] = update(id, completed = true)

  def uncheck(id: String): State[TodoList, Unit] = update(id, completed = false)

  def update(id: String, completed: Boolean): State[TodoList, Unit] = State { todoList =>
    val newTodoList = todoList.todos.get(id)
      .map(todo => {
        todoList.copy(todos = todoList.todos.updated(id, todo.copy(completed = completed)))
      })
      .getOrElse(todoList)
    (newTodoList, ())
  }

  def isCompleted: State[TodoList, Boolean] = State { todoList =>
    (todoList, todoList.todos.forall { case (_, todo) => todo.completed })
  }

  def clear(): State[TodoList, Unit] = State { todoList =>
    (todoList.copy(todos = Map[String, Todo]()), ())
  }
}

object Main extends App {
  val todoList = (
    for {
      todo <- TodoList.add("Make lunch")
      _ <- TodoList.check(todo.id)
      isCompleted <- TodoList.isCompleted
      _ <- if (isCompleted) {
        TodoList.clear()
      } else {
        State.get[TodoList]
      }
    } yield ()
  ).runS(TodoList(Map())).value
}
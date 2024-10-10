package in.rcard.example

import in.rcard.raise4s.Strategies.{MapError, mapTo}
import in.rcard.raise4s.raises

sealed trait Error
case class UserNotFound(id: Int) extends Error
case object GenericError
type GenericError = GenericError.type

case class User(id: Int, name: String)

trait UserRepository:
  def findById(id: Int): User raises GenericError

class Users(repo: UserRepository):
  def findUserById(id: Int): User raises UserNotFound = {
    // Error mapping logic
    given (GenericError mapTo UserNotFound) = _ => UserNotFound(id)
    // Happy path logic
    repo.findById(id)
  }

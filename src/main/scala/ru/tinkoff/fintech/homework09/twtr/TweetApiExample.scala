package ru.tinkoff.fintech.homework09.twtr

import java.time.Instant
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex
import scala.concurrent._
import scala.concurrent.duration._

/**
  * Вам необходимо реализовать api для создания твиттов, получения твитта и лайка твитта
  *
  * Создание твитта:
  * На вход к вам поступает CreateTweetRequest, из которого вы должны создать объект Tweet, обязательно проверив длину
  * текста (а может быть потом появятся и другие проверки).
  * hashTags вычисляется из tweet.text, собирая все слова, начинающиеся с символа `#`.
  * tweet.id генерируется из `UUID.randomUUID.toString`.
  * При условии прохождения всех проверок твит сохраняется в базу данных (вы можете реализовать любой способ сохранения:
  * в памяти, запись в файл, или то, что вам захочется).
  * После выполнения всех операций должен вернуться созданный объект.
  *
  * Получение твитта:
  * На вход к вам поступает GetTweetRequest, вернуть вы должны объект tweet, если он найдем по id.
  *
  * Лайк твитта:
  * Должен обновлять количество лайков у твитта и возвращать новое значение.
  * Если твит не найдет, то должна возвращаться ошибка
  *
  *
  * Все функции должны возвращать значение типа Future[?]
  *
  *
  * Если же сложилось непоправимое(например обрушилась сеть),
  * то необходимо обработать это достойным образом.
  */

case class Tweet(id: String,
                 user: String,
                 text: String,
                 hashTags: Seq[String] = Seq.empty,
                 createdAt: Option[Instant] = None,
                 likes: Int)

case class CreateTweetRequest(text: String, user: String)
case class GetTweetRequest(id: String)
case class LikeRequest(id: String)

trait TweetStorage {
  def add(t:Tweet):Unit
  def get(id:String): Option[Tweet]
  def like(id:String): Int
}

class InMemoryStorage extends TweetStorage {
  private var storage: Map[String, Tweet] = Map[String, Tweet]()
  def getSize:Int = {
    storage.size
  }
  def add(t: Tweet){
    storage = storage + (t.id -> t)
  }
  def get(id:String): Option[Tweet] = {
    storage.get(id)
  }
  def like(id:String): Int = {
    try {
      var t = storage(id)
      val newT = t.copy(likes = t.likes+1)
      storage + (id->newT)
      newT.likes
    }
    catch {
      case e: java.util.NoSuchElementException => 0
    }
  }
}

class TweetApi(storage: TweetStorage) {
  implicit val ec = ExecutionContext.global

  def getHashtags(text: String): Seq[String] = {
    val hashtagPttrn: Regex = "#[0-9a-zA-Z]+".r
    val out = hashtagPttrn findAllIn text
    out.toList
  }

  def createTweet(request: CreateTweetRequest): Future[Option[Tweet]] = {
    val res : Future[Option[Tweet]] = Future {
      if (request.text.length < 256) {
      val t = Tweet(id = UUID.randomUUID.toString, user = request.user,
        text = request.text, hashTags = getHashtags(request.text), createdAt = Option(java.time.Instant.now()), likes = 0)
      storage.add(t)
      Some(t)
    }
    else {
      throw new RuntimeException("Tweet Is Too Long!")
      }
    }
    res
  }

  def getTweet(request: GetTweetRequest): Future[Option[Tweet]] = {
    val res : Future[Option[Tweet]] = Future {
      val t: Option[Tweet] = storage.get(request.id)
      t match {
        case Some(t) => Some(t)
        case None => throw new RuntimeException("Unable To Get Tweet!")
      }
    }
    res
  }

  def likeTweet(request: LikeRequest): Future[Option[Int]] = {
    val res: Future[Option[Int]] = Future {
      val likes = storage.like(request.id)
      Some(likes)
    }
    res
  }
}

object TweetApiExample extends App {
  implicit val ec = ExecutionContext.global
  val storage: TweetStorage = new InMemoryStorage {}
  val app = new TweetApi(storage)

  val request = CreateTweetRequest(user = "me", text = "Hello, world!")

  val res = for {
    r1 <- Await.result(app.createTweet(request), 5.seconds)
  } yield r1

  println(res.get)
}
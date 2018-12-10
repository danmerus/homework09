package ru.tinkoff.fintech.homework09.twtr

import java.time.Instant
import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}
import scala.util.matching.Regex


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

sealed trait Result[T]
final case class Success[T](value: T) extends Result[T]
final case class Error[T](message: String) extends Result[T]

trait TweetStorage {
  def putTweet(tweet: Tweet): Future[Result[Tweet]]
  def getTweet(id: String): Future[Result[Tweet]]
  def updateTweet(tweet: Tweet): Future[Result[Tweet]]
}

class InMemoryStorage extends TweetStorage {
  implicit val ec = ExecutionContext.global
  private var storage: Map[String, Tweet] = Map.empty

  def getSize:Int = {
    storage.size
  }

  override def putTweet(t: Tweet): Future[Result[Tweet]] = Future {
    storage.get(t.id) match {
      case Some(e) => Error("Tweet already in storage")
      case None => storage += (t.id -> t)
                   Success(t)
    }
  }

  override def getTweet(id:String): Future[Result[Tweet]] = Future {
    storage.get(id) match {
      case Some(tweet) => Success(tweet)
      case _ => Error("No such tweet")
    }
  }

  override def updateTweet(tweet: Tweet): Future[Result[Tweet]] = Future {
    storage.get(tweet.id) match {
      case Some(_) =>
        storage += (tweet.id -> tweet)
        Success(tweet)
      case None => Error("Tweet is not in storage")
    }
  }
}

class TweetApi(storage: TweetStorage) {
  implicit val ec = ExecutionContext.global

  val MAX_TWEET_LENGTH = 256

  def getHashtags(text: String): Seq[String] = {
    val hashtagPttrn: Regex = "#[0-9a-zA-Z]+".r
    val out = hashtagPttrn findAllIn text
    out.toList
  }

  def createTweet(request: CreateTweetRequest): Future[Result[Tweet]] = {
    if (request.text.length < MAX_TWEET_LENGTH) {
      val t = Tweet(id = UUID.randomUUID.toString, user = request.user,
        text = request.text, hashTags = getHashtags(request.text), createdAt = Some(java.time.Instant.now()), likes = 0)
      storage.putTweet(t)
    }
    else {
      Future(Error("too long!"))
    }
  }

  def getTweet(request: GetTweetRequest): Future[Result[Tweet]] = {
    storage.getTweet(request.id)
  }

  def likeTweet(request: LikeRequest): Future[Result[Tweet]] = {
    storage.getTweet(request.id) flatMap {
      case Success(tweet) =>
        storage.updateTweet(tweet.copy(likes = tweet.likes + 1))
      case Error(message) => Future(Error(message))
    }
  }
}

object TweetApiExample extends App {

}
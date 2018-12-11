package ru.tinkoff.fintech.homework09

import org.scalatest.Inside._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{AsyncFlatSpec, Matchers}
import ru.tinkoff.fintech.homework09.twtr._

import scala.concurrent._
import scala.concurrent.duration._

class TweetAppSpec extends AsyncFlatSpec with Matchers with ScalaFutures {
  val text1 = "Hello, world! #Peace"
  val userName = "me"

  "TwitterApi" should "create tweet correctly" in {
    val tweetApi = new TweetApi(new InMemoryStorage())
    val request = CreateTweetRequest(text1, userName)
    val req = tweetApi.createTweet(request)
    val res = Await.result(req, 10 seconds)
    res match {
      case Success(tweet) => tweet.user shouldEqual "me"
        tweet.text shouldEqual text1
        tweet.hashTags shouldEqual List("#Peace")
    }
  }

  it should "error if tweet too long" in {
    val tweetApi = new TweetApi(new InMemoryStorage())
    val txt: String = "L"*257
    val request = CreateTweetRequest(txt, userName)
    val req = tweetApi.createTweet(request)
    val res = Await.result(req, 10 seconds)
    inside(res) {
      case Error(message) => message should be ("too long!")
    }
  }

  it should "like tweet" in {
    val tweetApi = new TweetApi(new InMemoryStorage())
    val request = CreateTweetRequest(text1, userName)
    val req = tweetApi.createTweet(request)
    val res = Await.result(req, 10 seconds)
    res match {
      case Success(tweet) => tweetApi.likeTweet(LikeRequest(tweet.id)) map { response => {
        response match {
          case Success(tweet) => tweet.likes shouldEqual 1
        }
      }
      }
    }
  }

  it should "return tweet by id" in {
    val tweetApi = new TweetApi(new InMemoryStorage())
    val request = CreateTweetRequest(text1, userName)
    val req = tweetApi.createTweet(request)
    val res = Await.result(req, 10 seconds)
    res match {
      case Success(tweet) => val ft = tweetApi.getTweet(GetTweetRequest(tweet.id))
        val t = Await.result(ft, 10 seconds)
        t match {
          case Success(tweet2) => tweet2 shouldEqual tweet
        }

    }
  }

  it should "return error if not in Storage" in {
    val tweetApi = new TweetApi(new InMemoryStorage())
    val nonexistentId = "22284848"
    val messageError = "No such tweet"
    val req = tweetApi.getTweet(GetTweetRequest(nonexistentId))
    val res = Await.result(req, 10 seconds)
    inside(res) {
      case Error(message) => message should be ("No such tweet")
    }
  }

}

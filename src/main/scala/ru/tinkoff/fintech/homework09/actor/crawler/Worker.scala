package ru.tinkoff.fintech.homework09.actor.crawler
import akka.actor.{Actor, ActorLogging, ActorRef}

import scala.collection.mutable.Queue
import scala.util.{Failure, Success}

class Worker(http: Http, parser: Parsr, master: ActorRef) extends Actor with ActorLogging {
  implicit val ec = context.dispatcher
  private var isBusy = false
  private val queue = Queue.empty[Url]

  private def toMater(url: Url, result: List[Url]): Unit = {
    master.tell(CrawlResult(url, result), master)
    isBusy = false
    getUrls()
  }

  private def getUrls(): Unit =
    if (!isBusy && queue.nonEmpty) {
      isBusy = true
      val url = queue.dequeue()
      http.get(url).map(b => parser.links(b)).onComplete {
        case Success(urls) => toMater(url, urls)
        case Failure(_)    => toMater(url, Nil)
      }
    }

  override def receive: Receive = {
    case Crawl(url) =>
      queue += url
      getUrls()
  }
}
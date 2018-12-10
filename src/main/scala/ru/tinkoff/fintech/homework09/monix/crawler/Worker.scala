package ru.tinkoff.fintech.homework09.monix.crawler
import monix.eval.{Fiber, Task}

trait Worker {
  def http: Http

  def parseLinks: Parsr

  def worker(workerQueue: MQueue[Url], crawlerQueue: MQueue[CrawlerMessage]): Task[Fiber[Unit]] = {
    def loop(taskUrl: Task[Url]): Task[Unit] = {
      taskUrl.flatMap { url =>
        http.get(url)
          .map(parseLinks.links)
          .onErrorRecover { case e: Exception => Nil }
          .flatMap(urls => crawlerQueue.offer(CrawlResult(url, urls)))
          .flatMap((_) => loop(workerQueue.take))
      }
    }
    loop(workerQueue.take).start

  }
}
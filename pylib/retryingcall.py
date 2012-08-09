from twisted.internet import reactor, defer, task
from twisted.python import log, failure
from operator import mul
from functools import partial

class RetryingCall(object):
    """Calls a function repeatedly, passing it args and kw args. Failures
    are passed to a user-supplied failure testing function. If the failure
    is ignored, the function is called again after a delay whose duration
    is obtained from a user-supplied iterator. The start method (below)
    returns a deferred that fires with the eventual non-error result of
    calling the supplied function, or fires its errback if no successful
    result can be obtained before the delay backoff iterator raises
    StopIteration.
    """
    def __init__(self, f, *args, **kw):
        self._f = f
        self._args = args
        self._kw = kw
        
    def _err(self, fail):
        try:
            fail = self._failureTester(fail)
        except:
            self._deferred.errback()
        else:
            if isinstance(fail, failure.Failure):
                self._deferred.errback(fail)
            else:
                log.msg('RetryingCall: Ignoring %r' % (fail,))
                self._call()

    def _call(self):
        try:
            delay = self._backoffIterator.next()
        except StopIteration:
            log.msg('StopIteration in RetryingCall: ran out of attempts.')
            self._deferred.errback()
        else:
            d = task.deferLater(reactor, delay,
                                self._f, *self._args, **self._kw)
            d.addCallbacks(self._deferred.callback, self._err)

    def start(self, backoffIterator=None, failureTester=None):
        self._backoffIterator = backoffIterator or simpleBackoffIterator()
        self._failureTester = failureTester or (lambda f: f)
        self._deferred = defer.Deferred()
        self._call()
        return self._deferred

def simpleBackoffIterator(maxResults=10, maxDelay=5.0, now=True,
                          initDelay=0.01, incFunc=None):
    assert maxResults > 0
    remaining = maxResults
    delay = initDelay
    incFunc = incFunc or partial(mul, 2.0)
    
    if now:
        yield 0.0
        remaining -= 1
    while True:
        if remaining == 0:
            raise StopIteration
        yield (delay if delay < maxDelay else maxDelay)
        delay = incFunc(delay)
        remaining -= 1

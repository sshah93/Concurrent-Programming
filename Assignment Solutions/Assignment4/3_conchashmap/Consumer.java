import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;


public class Consumer implements Runnable {

    private BlockingQueue<Page> sharedQueue;
    private ConcurrentHashMap<String, Integer> wordFreq;

    public Consumer(BlockingQueue<Page> q, ConcurrentHashMap<String, Integer> countMap) {
	sharedQueue = q;
	wordFreq = countMap;
    }

    public void run() {
	try {
	    while (true) {
		Page pg = sharedQueue.take();
		if (pg.isPoisonPill())
		    break;
		Iterable<String> allWords = new Words(pg.getText());
		for (String s: allWords)
		    countWord(s);
	    }
	} catch (Exception ex) {
	}
    }

    private void countWord(String w) {
	while (true) {
	    Integer currentCount = wordFreq.get(w);
	    if (currentCount == null) {
		if (wordFreq.putIfAbsent(w, 1) == null)
		    break;
	    } else {
		if (wordFreq.replace(w, currentCount, currentCount + 1))
		    break;
	    }
	}
    }
}

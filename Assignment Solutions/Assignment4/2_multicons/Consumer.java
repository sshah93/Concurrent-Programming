import java.util.concurrent.BlockingQueue;
import java.util.Map;


public class Consumer implements Runnable {

    private BlockingQueue<Page> sharedQueue;
    private Map<String, Integer> wordFreq;

    public Consumer(BlockingQueue<Page> q, Map<String, Integer> countMap) {
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
	synchronized(wordFreq) {
	    try {
		Integer currentCount = wordFreq.get(w);
		if (currentCount == null)
		    wordFreq.put(w, 1);
		else
		    wordFreq.put(w, currentCount + 1);
	    } catch (Exception ex) {
	    }
	}
    }
}

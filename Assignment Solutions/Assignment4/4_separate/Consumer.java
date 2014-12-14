import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Map;


public class Consumer implements Runnable {

    private BlockingQueue<Page> sharedQueue;
    private ConcurrentHashMap<String, Integer> wordFreq;
    private ConcurrentHashMap<String, Integer> overallCount;

    public Consumer(BlockingQueue<Page> q, ConcurrentHashMap<String, Integer> overall) {
	sharedQueue = q;
	wordFreq = new ConcurrentHashMap<String, Integer>();
	overallCount = overall;
    }

    public void run() {
	try {
	    while (true) {
		Page pg = sharedQueue.take();
		if (pg.isPoisonPill()) {
		    mergeCount();
		    break;
		}
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

    private void mergeCount() {
	for (Map.Entry<String, Integer> e : wordFreq.entrySet()) {
	    String word = e.getKey();
	    Integer count = e.getValue();
	    while (true) {
		Integer total = overallCount.get(word);
		if (total == null) {
		    if (overallCount.putIfAbsent(word, count) == null)
			break;
		} else {
		    if (overallCount.replace(word, total, total+count))
			break;
		}
	    }
	}
    }

}

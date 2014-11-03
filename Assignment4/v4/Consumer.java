import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Suketu
 *
 */
public class Consumer implements Runnable {
	// class attributes
	private BlockingQueue<Page> sharedQueue;
	private ConcurrentHashMap<String, Integer> tokenFreq;
	private ConcurrentHashMap<String, Integer> sharedTokenFreq;
	
	// constructor
	public Consumer(ArrayBlockingQueue<Page> q, ConcurrentHashMap<String, Integer> t) {
		sharedQueue = q;
		sharedTokenFreq = t;
		tokenFreq = new ConcurrentHashMap<String, Integer>();
	}

	@Override
	public void run() {
		Page pg;
		// parse through all the pages
		while (true) {
			try {
				pg = sharedQueue.take();
	
				// check for poison pill
				if (pg.isPoisonPill()) {
					break;
				}

				// count tokens for each page
				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					countToken(s);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		
		// merge the concurrenthashmaps into 1 concurrenthashmap
		for(Map.Entry<String, Integer> entry: tokenFreq.entrySet()) {
			Integer old_value = sharedTokenFreq.putIfAbsent(entry.getKey(), entry.getValue());
			Integer new_value;
			
			if(old_value != null) {
				do {
					old_value = sharedTokenFreq.get(entry.getKey());
					new_value = old_value + entry.getValue();
				} while(!sharedTokenFreq.replace(entry.getKey(), old_value, new_value));
			}
		}
	}
	
	private void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if(currentCount == null) {
			tokenFreq.put(tok, 1);
		}
		else {
			tokenFreq.put(tok, currentCount + 1);
		}
	}
}
import java.util.HashMap;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * @author Suketu
 *
 */
public class Consumer implements Runnable {

	// sharedQueue for both Producer and Consumer
	private BlockingQueue<Page> sharedQueue;
	// local copy of the hashmap
	private HashMap<String, Integer> tokenFreq;

	// constructor that takes the queue and hashmap
	public Consumer(ArrayBlockingQueue<Page> q, HashMap<String, Integer> t) {
		sharedQueue = q;
		tokenFreq = t;
	}

	@Override
	public void run() {
		Page pg;
		while (true) {
			try {
				pg = sharedQueue.take();

				if (pg.isPoisonPill()) {
					break;
				}

				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					countToken(s);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	
	// local copy of countToken method
	private void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if (currentCount == null)
			tokenFreq.put(tok, 1);
		else
			tokenFreq.put(tok, currentCount + 1);
	}
}
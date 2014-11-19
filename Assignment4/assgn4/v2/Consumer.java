import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * @author Suketu
 *
 */
public class Consumer implements Runnable {

	// sharedQueue for both Producer and Consumer
	private BlockingQueue<Page> sharedQueue;

	// constructor that takes the queue and hashmap
	public Consumer(ArrayBlockingQueue<Page> q) {
		sharedQueue = q;
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
					TokenCount.countToken(s);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	
	/*// local copy of countToken method
	private void countToken(String tok) {
		Integer currentCount = tokenFreq.get(tok);
		if (currentCount == null)
			tokenFreq.put(tok, 1);
		else
			tokenFreq.put(tok, currentCount + 1);
	}*/
}
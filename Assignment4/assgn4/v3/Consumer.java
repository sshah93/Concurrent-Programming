import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author Suketu
 *
 */
public class Consumer implements Runnable {

	private BlockingQueue<Page> sharedQueue;
	private ConcurrentHashMap<String, Integer> tokenFreq;
	
	public Consumer(ArrayBlockingQueue<Page> q, ConcurrentHashMap<String, Integer> t) {
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
	
	private void countToken(String tok) {
		Integer old = tokenFreq.putIfAbsent(tok, 1);
		Integer val;
		
		if(old != null) {
			do {
				old = tokenFreq.get(tok);
				val = old + 1;
			} while (!tokenFreq.replace(tok, old, val));
		}
	}
}
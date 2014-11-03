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
	
	// constructor
	public Consumer(ArrayBlockingQueue<Page> q, ConcurrentHashMap<String, Integer> t) {
		sharedQueue = q;
		tokenFreq = t;
	}

	@Override
	public void run() {
		Page pg;
		// iterates over each page of the shared queue
		while (true) {
			try {
				pg = sharedQueue.take();

				// if poison pill is found then we know we are at the end
				if (pg.isPoisonPill()) {
					break;
				}

				// for each page calls the count token method
				Iterable<String> allTokens = new Words(pg.getText());
				for (String s : allTokens) {
					countToken(s);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
	
	// count token method part of the class itself
	private void countToken(String tok) {
		Integer old = tokenFreq.putIfAbsent(tok, 1);
		Integer val;
		
		// check if the value of the token has been replaced by someother value
		// if it isn't then update the value
		// if it is then check again and modify if necessary
		if(old != null) {
			do {
				old = tokenFreq.get(tok);
				val = old + 1;
			} while (!tokenFreq.replace(tok, old, val));
		}
	}
}
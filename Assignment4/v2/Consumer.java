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
					TokenCount.countToken(s);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
}
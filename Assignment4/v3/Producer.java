import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * @author Suketu
 *
 */
public class Producer implements Runnable {
	// class attributes
	private String fileName;
	private BlockingQueue<Page> sharedQueue;
	private Integer numPages;
	private int numPoisonPills;

	// constructor
	public Producer(ArrayBlockingQueue<Page> q, String f, Integer num, int num_consumer_threads) {
		sharedQueue = q;
		fileName = f;
		numPages = num;
		numPoisonPills = num_consumer_threads;
	}

	/**
	 * @param args
	 */

	@Override
	public void run() {
		// parse XML into pages
		Iterable<Page> allPages = new Pages(numPages, fileName);
		try {
			for (Page pg : allPages) {
				sharedQueue.put(pg);
			}
			
			// put the same amount of poison pills as the number of consumers
			for (int i = 0; i <= numPoisonPills; i++){
				sharedQueue.put(new PoisonPill());
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}
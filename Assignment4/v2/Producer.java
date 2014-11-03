import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

/**
 * 
 */

/**
 * @author Suketu
 *
 */
public class Producer implements Runnable {
	
	private String fileName;
	private BlockingQueue<Page> sharedQueue;
	private Integer numPages;
	private int numConsumers;

	public Producer(ArrayBlockingQueue<Page> q, String f, Integer num, int cons) {
		sharedQueue = q;
		fileName = f;
		numPages = num;
		numConsumers = cons;
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
			
			// passes the same number of poison pills as the number of consumers
			// 1 poison pill for each consumer thread
			for (int i = 0; i < numConsumers; i++) {
				sharedQueue.put(new PoisonPill());
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}
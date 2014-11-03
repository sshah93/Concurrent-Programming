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
	// class attributes
	private String fileName;
	private BlockingQueue<Page> sharedQueue;
	private Integer numPages;

	// constructor
	public Producer(ArrayBlockingQueue<Page> q, String f, Integer num) {
		sharedQueue = q;
		fileName = f;
		numPages = num;
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
			
			// pass the poison pill to suggest the end of sharedqueue to the consumer
			sharedQueue.put(new PoisonPill());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}

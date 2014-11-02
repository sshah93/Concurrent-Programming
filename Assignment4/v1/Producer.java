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
			sharedQueue.put(new PoisonPill());
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

}

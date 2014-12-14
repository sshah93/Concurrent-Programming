import java.util.concurrent.BlockingQueue;


public class Producer implements Runnable {

    private BlockingQueue<Page> sharedQueue;
    private Integer numPages;
    private String XMLFileName;

    public Producer (BlockingQueue<Page> q, Integer n, String file) {
	sharedQueue = q;
	numPages = n;
	XMLFileName = file;
    }

    public void run() {
	try {
	    Iterable<Page> allPages = new Pages(numPages, XMLFileName);
	    for (Page pg: allPages)
		sharedQueue.put(pg);
	} catch (Exception ex) {
	}
    }
}


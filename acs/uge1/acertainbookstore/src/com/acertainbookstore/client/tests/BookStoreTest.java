package com.acertainbookstore.client.tests;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.acertainbookstore.business.Book;
import com.acertainbookstore.business.BookCopy;
import com.acertainbookstore.business.CertainBookStore;
import com.acertainbookstore.business.ImmutableStockBook;
import com.acertainbookstore.business.StockBook;
import com.acertainbookstore.business.BookRating;
import com.acertainbookstore.client.BookStoreHTTPProxy;
import com.acertainbookstore.client.StockManagerHTTPProxy;
import com.acertainbookstore.interfaces.BookStore;
import com.acertainbookstore.interfaces.StockManager;
import com.acertainbookstore.utils.BookStoreConstants;
import com.acertainbookstore.utils.BookStoreException;

/**
 * {@link BookStoreTest} tests the {@link BookStore} interface.
 *
 * @see BookStore
 */
public class BookStoreTest {

	/** The Constant TEST_ISBN. */
	private static final int TEST_ISBN  = 3044560;
	private static final int TEST_ISBN2 = TEST_ISBN + 1;
	private static final int TEST_ISBN3 = TEST_ISBN + 2;
	private static final int TEST_ISBN4 = TEST_ISBN + 3;
	private static final int TEST_ISBN5 = TEST_ISBN + 4;

	/** The Constant NUM_COPIES. */
	private static final int NUM_COPIES = 5;

	/** The local test. */
	private static boolean localTest = false;

	/** The store manager. */
	private static StockManager storeManager;

	/** The client. */
  private static BookStore client;

	/**
	 * Sets the up before class.
	 */
	@BeforeClass
	public static void setUpBeforeClass() {
		try {
			String localTestProperty = System.getProperty(BookStoreConstants.PROPERTY_KEY_LOCAL_TEST);
			localTest = (localTestProperty != null) ? Boolean.parseBoolean(localTestProperty) : localTest;

			if (localTest) {
				CertainBookStore store = new CertainBookStore();
				storeManager = store;
				client = store;
			} else {
				storeManager = new StockManagerHTTPProxy("http://localhost:8081/stock");
				client = new BookStoreHTTPProxy("http://localhost:8081");
			}

			storeManager.removeAllBooks();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Helper method to add some books.
	 *
	 * @param isbn
	 *            the isbn
	 * @param copies
	 *            the copies
	 * @throws BookStoreException
	 *             the book store exception
	 */
	public void addBooks(int isbn, int copies) throws BookStoreException {
		Set<StockBook> booksToAdd = new HashSet<>();
		StockBook book = new ImmutableStockBook(isbn, "Test of Thrones", "George RR Testin'", (float) 10, copies, 0, 0,
				0, false);
		booksToAdd.add(book);
		storeManager.addBooks(booksToAdd);
	}

	/**
	 * Helper method to get the default book used by initializeBooks.
	 *
	 * @return the default book
	 */
	public StockBook getDefaultBook() {
		return new ImmutableStockBook(TEST_ISBN, "Harry Potter and JUnit", "JK Unit", (float) 10, NUM_COPIES, 0, 0, 0,
				false);
	}
	public StockBook getDefaultBook2() {
		return new ImmutableStockBook(TEST_ISBN2, "foo title",
						"foo author", (float) 42, NUM_COPIES, 0, 0, 0, false);
	}
	public StockBook getDefaultBook3() {
		return new ImmutableStockBook(TEST_ISBN3, "bar title",
						"bar author", (float) 42, NUM_COPIES, 0, 0, 0, false);
	}
	public StockBook getDefaultBook4() {
		return new ImmutableStockBook(TEST_ISBN4, "book 4",
						"yet another author", (float) 42, NUM_COPIES, 0, 0, 0, false);
	}
	public StockBook getDefaultBook5() {
		return new ImmutableStockBook(TEST_ISBN5, "book 5",
						"who writes these books anyway", (float) 40000, NUM_COPIES, 0, 0, 0, false);
	}

	/**
	 * Method to add a book, executed before every test case is run.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Before
	public void initializeBooks() throws BookStoreException {
		Set<StockBook> booksToAdd = new HashSet<>();
		booksToAdd.add(getDefaultBook());
		storeManager.addBooks(booksToAdd);
	}

	public void initializeMoreBooks() throws BookStoreException {
		Set<StockBook> booksToAdd = new HashSet<>();
		booksToAdd.add(getDefaultBook2());
		booksToAdd.add(getDefaultBook3());
		booksToAdd.add(getDefaultBook4());
		booksToAdd.add(getDefaultBook5());
		storeManager.addBooks(booksToAdd);
	}

	/**
	 * Method to clean up the book store, execute after every test case is run.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@After
	public void cleanupBooks() throws BookStoreException {
		storeManager.removeAllBooks();
	}

	/**
	 * Tests basic buyBook() functionality.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testBuyAllCopiesDefaultBook() throws BookStoreException {
		// Set of books to buy
		Set<BookCopy> booksToBuy = new HashSet<>();
		booksToBuy.add(new BookCopy(TEST_ISBN, NUM_COPIES));

		// Try to buy books
		client.buyBooks(booksToBuy);

		List<StockBook> listBooks = storeManager.getBooks();
		assertTrue(listBooks.size() == 1);
		StockBook bookInList = listBooks.get(0);
		StockBook addedBook = getDefaultBook();

		assertTrue(bookInList.getISBN() == addedBook.getISBN() && bookInList.getTitle().equals(addedBook.getTitle())
				&& bookInList.getAuthor().equals(addedBook.getAuthor()) && bookInList.getPrice() == addedBook.getPrice()
				&& bookInList.getNumSaleMisses() == addedBook.getNumSaleMisses()
				&& bookInList.getAverageRating() == addedBook.getAverageRating()
				&& bookInList.getNumTimesRated() == addedBook.getNumTimesRated()
				&& bookInList.getTotalRating() == addedBook.getTotalRating()
				&& bookInList.isEditorPick() == addedBook.isEditorPick());
	}

	/**
	 * Tests that books with invalid ISBNs cannot be bought.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testBuyInvalidISBN() throws BookStoreException {
		List<StockBook> booksInStorePreTest = storeManager.getBooks();

		// Try to buy a book with invalid ISBN.
		HashSet<BookCopy> booksToBuy = new HashSet<>();
		booksToBuy.add(new BookCopy(TEST_ISBN, 1)); // valid
		booksToBuy.add(new BookCopy(-1, 1)); // invalid

		// Try to buy the books.
		try {
			client.buyBooks(booksToBuy);
			fail();
		} catch (BookStoreException ignored) {
			;
		}

		List<StockBook> booksInStorePostTest = storeManager.getBooks();

		// Check pre and post state are same.
		assertTrue(booksInStorePreTest.containsAll(booksInStorePostTest)
				&& booksInStorePreTest.size() == booksInStorePostTest.size());
	}

	/**
	 * Tests that books can only be bought if they are in the book store.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testBuyNonExistingISBN() throws BookStoreException {
		List<StockBook> booksInStorePreTest = storeManager.getBooks();

		// Try to buy a book with ISBN which does not exist.
		HashSet<BookCopy> booksToBuy = new HashSet<BookCopy>();
		booksToBuy.add(new BookCopy(TEST_ISBN, 1)); // valid
		booksToBuy.add(new BookCopy(100000, 10));   // invalid

		// Try to buy the books.
		try {
			client.buyBooks(booksToBuy);
			fail();
		} catch (BookStoreException ignored) {
			;
		}

		List<StockBook> booksInStorePostTest = storeManager.getBooks();

		// Check pre and post state are same.
		assertTrue(booksInStorePreTest.containsAll(booksInStorePostTest) &&
						   booksInStorePostTest.containsAll(booksInStorePreTest));
	}

	/**
	 * Tests that you can't buy more books than there are copies.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testBuyTooManyBooks() throws BookStoreException {
		List<StockBook> booksInStorePreTest = storeManager.getBooks();

		// Try to buy more copies than there are in store.
		HashSet<BookCopy> booksToBuy = new HashSet<BookCopy>();
		booksToBuy.add(new BookCopy(TEST_ISBN, NUM_COPIES + 1));

		try {
			client.buyBooks(booksToBuy);
			fail();
		} catch (BookStoreException ignored) {
			;
		}

		List<StockBook> booksInStorePostTest = storeManager.getBooks();
		assertTrue(booksInStorePreTest.containsAll(booksInStorePostTest)
				&& booksInStorePreTest.size() == booksInStorePostTest.size());
	}

	/**
	 * Tests that you can't buy a negative number of books.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testBuyNegativeNumberOfBookCopies() throws BookStoreException {
		List<StockBook> booksInStorePreTest = storeManager.getBooks();

		// Try to buy a negative number of copies.
		HashSet<BookCopy> booksToBuy = new HashSet<BookCopy>();
		booksToBuy.add(new BookCopy(TEST_ISBN, -1));

		try {
			client.buyBooks(booksToBuy);
			fail();
		} catch (BookStoreException ex) {
			;
		}

		List<StockBook> booksInStorePostTest = storeManager.getBooks();
		assertTrue(booksInStorePreTest.containsAll(booksInStorePostTest)
				&& booksInStorePreTest.size() == booksInStorePostTest.size());
	}

	/**
	 * Tests that all books can be retrieved.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testGetBooks() throws BookStoreException {
		Set<StockBook> booksAdded = new HashSet<>();
		booksAdded.add(getDefaultBook());

		Set<StockBook> booksToAdd = new HashSet<>();
		booksToAdd.add(new ImmutableStockBook(TEST_ISBN + 1, "The Art of Computer Programming", "Donald Knuth",
				(float) 300, NUM_COPIES, 0, 0, 0, false));
		booksToAdd.add(new ImmutableStockBook(TEST_ISBN + 2, "The C Programming Language",
				"Dennis Ritchie and Brian Kerninghan", (float) 50, NUM_COPIES, 0, 0, 0, false));

		booksAdded.addAll(booksToAdd);

		storeManager.addBooks(booksToAdd);

		// Get books in store.
		List<StockBook> listBooks = storeManager.getBooks();

		// Make sure the lists equal each other.
		assertTrue(listBooks.containsAll(booksAdded) && listBooks.size() == booksAdded.size());
	}

	/**
	 * Tests that a list of books with a certain feature can be retrieved.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testGetCertainBooks() throws BookStoreException {
		Set<StockBook> booksToAdd = new HashSet<>();
		booksToAdd.add(new ImmutableStockBook(TEST_ISBN + 1, "The Art of Computer Programming", "Donald Knuth",
				(float) 300, NUM_COPIES, 0, 0, 0, false));
		booksToAdd.add(new ImmutableStockBook(TEST_ISBN + 2, "The C Programming Language",
				"Dennis Ritchie and Brian Kernighan", (float) 50, NUM_COPIES, 0, 0, 0, false));

		storeManager.addBooks(booksToAdd);

		// Get a list of ISBNs to retrieved.
		Set<Integer> isbnList = new HashSet<>();
		isbnList.add(TEST_ISBN + 1);
		isbnList.add(TEST_ISBN + 2);

		// Get books with that ISBN.
		List<Book> books = client.getBooks(isbnList);

		// Make sure the lists equal each other
		assertTrue(books.containsAll(booksToAdd) && books.size() == booksToAdd.size());
	}

	/**
	 * Tests that books cannot be retrieved if ISBN is invalid.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@Test
	public void testGetInvalidIsbn() throws BookStoreException {
		List<StockBook> booksInStorePreTest = storeManager.getBooks();

		// Make an invalid ISBN.
		HashSet<Integer> isbnList = new HashSet<>();
		isbnList.add(TEST_ISBN); // valid
		isbnList.add(-1); // invalid

		HashSet<BookCopy> booksToBuy = new HashSet<>();
		booksToBuy.add(new BookCopy(TEST_ISBN, -1));

		try {
			client.getBooks(isbnList);
			fail();
		} catch (BookStoreException ignored) {
			;
		}

		List<StockBook> booksInStorePostTest = storeManager.getBooks();
		assertTrue(booksInStorePreTest.containsAll(booksInStorePostTest)
				&& booksInStorePreTest.size() == booksInStorePostTest.size());
	}

	/**
	 * Tear down after class.
	 *
	 * @throws BookStoreException
	 *             the book store exception
	 */
	@AfterClass
	public static void tearDownAfterClass() throws BookStoreException {
		storeManager.removeAllBooks();

		if (!localTest) {
			((BookStoreHTTPProxy) client).stop();
			((StockManagerHTTPProxy) storeManager).stop();
		}
	}

	@Test
	public void testRateBooks() throws BookStoreException {

		Set<StockBook> booksToAdd = new HashSet<>();
		booksToAdd.add(getDefaultBook2());

		// add books and assert that they have no rating.
		storeManager.addBooks(booksToAdd);
		storeManager.getBooks().forEach(book -> assertEquals(0, book.getTotalRating()));


		// create some ratings ...
		int rating = 3;
		Set<BookRating> ratings = new HashSet<>();
		ratings.add(new BookRating(TEST_ISBN2, rating));

		// ... and make those ratings!
		client.rateBooks(ratings);


		// fetch newly rated books.
		List<StockBook> rated_books = storeManager.getBooks();
		StockBook book0_rated = rated_books.get(0);
		StockBook book2_rated = rated_books.get(1);

		// assert that book 0 is still unrated, and that
		// book 2 has properly received the given rating.
		assertEquals(0,      book0_rated.getTotalRating());
		assertEquals(rating, book2_rated.getTotalRating());
	}

	@Test(expected = BookStoreException.class)
	public void testRateBooksInvalidIsbn() throws BookStoreException {

	  // create a BookRating with an invalid isbn.
		Set<BookRating> ratings = new HashSet<>();
		int valid_rating = 3;
		int invalid_isbn = -100;
		ratings.add(new BookRating(invalid_isbn, valid_rating));

		// then make that rating. this should fail with a BookStoreException.
		client.rateBooks(ratings);
	}

	@Test(expected = BookStoreException.class)
	public void testRateBooksInvalidRating() throws BookStoreException {

		// create a BookRating with an invalid rating.
		Set<BookRating> ratings = new HashSet<>();
		int invalid_rating = -42;
		ratings.add(new BookRating(TEST_ISBN, invalid_rating));

		// then make that rating. this should fail with a BookStoreException.
		client.rateBooks(ratings);
	}

	@Test
	public void testRateBooksAllOrNothing() throws BookStoreException {

		// add a couple more default books.
		initializeMoreBooks();

		// assert that no book has been rated yet.
		storeManager.getBooks().forEach(book -> assertEquals(0, book.getTotalRating()));

		Set<BookRating> ratings = new HashSet<>();
		// create one valid and one invalid BookRating.
		int valid_rating = 3;
		int invalid_rating = -42;
		ratings.add(new BookRating(TEST_ISBN, valid_rating));
		ratings.add(new BookRating(TEST_ISBN2, invalid_rating));

		// register the new ratings. this fails, since the second rating is invalid.
    try {
			client.rateBooks(ratings);
		}
		catch (BookStoreException ignored) {
		}

    // assert all-or-nothing semantics by verifying that all books still have rating 0.
    storeManager.getBooks().forEach(book -> assertEquals(0, book.getTotalRating()));
	}

	@Test(expected = BookStoreException.class)
	public void testRateBooksInvalidRatingAndIsbn() throws BookStoreException {

		// create a BookRating with both invalid rating and isbn.
		Set<BookRating> ratings = new HashSet<>();
		int invalid_rating = 7;
		int invalid_isbn = -42;
		ratings.add(new BookRating(invalid_isbn, invalid_rating));

		// then make that rating. this should fail with a BookStoreException.
		client.rateBooks(ratings);
	}

	@Test(expected = BookStoreException.class)
	public void testRateBooksNonExistingIsbn() throws BookStoreException {
		// create a BookRating with a valid rating, and an ISBN which is
		// valid but which refers to a book which does not exist in the bookstore.
		Set<BookRating> ratings = new HashSet<>();
		int valid_rating = 3;
		int unknown_isbn = 42;
		ratings.add(new BookRating(unknown_isbn, valid_rating));

		// then make that rating. this should fail with a BookStoreException.
		client.rateBooks(ratings);
	}


	@Test
	public void testGetTopRatedBooksForKLargerThanNumberOfBooksInStore() throws BookStoreException {

		// add a couple more default books.
		initializeMoreBooks();

		// send some empty ratings.
		Set<BookRating> emptyRatings = new HashSet<>();
		client.rateBooks(emptyRatings);

		// ask for top 10 books. As explained in the report, I have made the assumption
		// that if k is larger than the number of books in the book store, then all books
		// are trivially in the top k best rated books.
		int k = 10;
		List <Book> kTopRatedBooks = client.getTopRatedBooks(k);
		List<StockBook> allBooks = storeManager.getBooks();

		// assert that the k=10 top rated books
		// are simply all the books in the store.
		assertTrue(kTopRatedBooks.containsAll(allBooks) &&
						   allBooks.containsAll(kTopRatedBooks));
	}


	@Test
	public void testGetTopRatedBooks() throws BookStoreException {

		// add a couple more default books.
    initializeMoreBooks();

    // rate each book in the store.
		Set<BookRating> ratings = new HashSet<>();
		ratings.add(new BookRating(TEST_ISBN,  3));
		ratings.add(new BookRating(TEST_ISBN2, 2));
		ratings.add(new BookRating(TEST_ISBN3, 0));
		ratings.add(new BookRating(TEST_ISBN4, 1));
		ratings.add(new BookRating(TEST_ISBN5, 4));
		client.rateBooks(ratings);

		// ask for the top 3 best rated books. These should be
		// TEST_ISBN5, TEST_ISBN, and TEST_ISBN2, in that order.
		int k = 3;

		List<Book> kTopRatedBooks = client.getTopRatedBooks(k);

		assertEquals(getDefaultBook5(), kTopRatedBooks.get(0));
		assertEquals(getDefaultBook(),  kTopRatedBooks.get(1));
		assertEquals(getDefaultBook2(), kTopRatedBooks.get(2));
	}

	@Test
	public void testGetTopRatedBooksMultipleBooksSameRating() throws BookStoreException {

		// add a couple more default books.
		initializeMoreBooks();

		// rate each book in the store.
		Set<BookRating> ratings = new HashSet<>();
		ratings.add(new BookRating(TEST_ISBN,  4));
		ratings.add(new BookRating(TEST_ISBN2, 4));
		ratings.add(new BookRating(TEST_ISBN3, 2));
		ratings.add(new BookRating(TEST_ISBN4, 2));
		ratings.add(new BookRating(TEST_ISBN5, 4));
		client.rateBooks(ratings);

		// ask for the top 3 best rated books. These should be
		// TEST_ISBN5, TEST_ISBN, and TEST_ISBN2, but in no
		// particular order.
		int k = 3;

		List<Book> kTopRatedBooks = client.getTopRatedBooks(k);
		List<StockBook> expected = new ArrayList<>();

		expected.add(getDefaultBook());
		expected.add(getDefaultBook2());
		expected.add(getDefaultBook5());

		// assert that the k top rated books are books 0, 2, and 5,
		// in no particular order, and nothing more.
		assertTrue(expected.containsAll(kTopRatedBooks) &&
					   	 kTopRatedBooks.containsAll(expected));
	}

}





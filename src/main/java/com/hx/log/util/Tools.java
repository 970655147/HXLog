/**
 * file name : Tools.java
 * created at : 6:58:34 PM Jul 25, 2015
 * created by 970655147
 */

package com.hx.log.util;

import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.concurrent.*;

import com.hx.log.biz.BizUtils;
import com.hx.log.collection.CollectionUtils;
import com.hx.log.collection.MapUtils;
import com.hx.log.date.DateUtils;
import com.hx.log.file.FileUtils;
import com.hx.log.file.TmpGetter;
import com.hx.log.idx.IdxUtils;
import com.hx.log.io.BufferManager;
import com.hx.log.json.JSONUtils;
import com.hx.log.biz.BizUtils.GetLengthStrMethod;
import com.hx.log.str.StringUtils;
import com.hx.log.io.BufferManager.BuffSizeEstimator;
import com.hx.log.io.BufferManager.BufferHandler;

import com.hx.json.JSONArray;
import com.hx.json.JSONObject;

// 工具类
public final class Tools {
	
	// disable constructor
	private Tools() {
		Tools.assert0("can't instantiate !");
	}
	
	// 常量
	public static final Random ran = new Random();
	public static final Character SLASH = Constants.SLASH;
	public static final Character INV_SLASH = Constants.INV_SLASH;
	public static final Character DOT = Constants.DOT;
	public static final Character COMMA = Constants.COMMA;
	public static final Character COLON = Constants.COLON;	
	public static final Character SPACE = Constants.SPACE;
	public static final Character TAB = Constants.TAB;
	public static final Character CR = Constants.CR;
	public static final Character LF = Constants.LF;
	public static final Character QUESTION = Constants.QUESTION;
	public static final Character QUOTE = Constants.QUOTE;
	public static final Character SINGLE_QUOTE = Constants.SINGLE_QUOTE;
	public static final String CRLF = Constants.CRLF;
	public static final String EMPTY_STR = Constants.EMPTY_STR;
	public static final String NULL = Constants.NULL;
	public static final String TRUE = Constants.TRUE;
	public static final String FALSE = Constants.FALSE;
	
	// 业务相关常量
	public static final String TASK = "task";
	public static final String SITE = "site";
	public static final String NAME = "name";
	public static final String URL = "url";
	public static final BigDecimal BIGDEC_ZERO = new BigDecimal("0.0");
	public static final Integer INTE_ZERO = new Integer("0");
	public static final String PAGE_NO = "pageNo";
	public static final String KEY_WORD = "keyWord";
	public static final String DEBUG_ENABLE = "debugEnable";
	public static final String FETCHED_RESULT = "fetchedResult";
	public static final String SPENT = "spent";
	public static final String DEPTH = "depth";
	public static final String REQUEST = "request";
	public static final String RESPONE = "response";
	public static final String REASON = "reason";
	public static final String MSG = "message";
	public static final String EXT = "ext";
	public static final String BUCKET = "bucket";
	public static final String BIN = "bin";
	// add at 2016.06.09
	public static final String FILTER = "filter";
	public static final String ASSERT = "assert";
	// add at 2016.03.21
	public static final String BRAND = "brand";
	public static final String UPC = "universalProductCode";
	public static final String MPN = "manufacturePartNumber";
	public static final String LEVEL = "level";
	public static final String VALUE = "value";
	public static final String DESCRIPTION = "description";
	public static final String LIST = "list";
	public static final String REBATE = "rebate";
	public static final String FINAL = "final";
	public static final String PRICE = "price";
	
	// http相关常量
	public static final String GET = "get";
	public static final String POST = "post";
	public static final String PUT = "put";
	public static final String DELETE = "delete";
	public static final String HEADER = "header";
	public static final String TRACE = "trace";
	
	public static final String COOKIE_STR = "Cookie";
	public static final String RESP_COOKIE_STR = "Set-Cookie";
	public static final String CONTENT_TYPE = "Content-Type";
	public static final String CONTENT_ENCODING = "Content-Encoding";
	public static final String ACCEPT = "Accept";
	public static final String ACCEPT_ENCODING = "Accept-Encoding";
	public static final String ACCEPT_LANGUAGE = "Accept-Language";
	public static final String CACHE_CONTROL = "Cache-Control";
	public static final String CONNECTION = "Connection";
	public static final String HOST = "Host";
	public static final String REFERER = "Referer";
	public static final String USER_AGENT = "User-Agent";
	public static final String DATE = "Date";
	public static final String SERVER = "Server";
	public static final String TRANSFER_ENCODING = "Transfer-Encoding";
	public static final String LAST_MODIFIED = "Last-Modified";
	public static final String IF_MODIFIED_SINCE = "If-Modified-Since";
	
//	 常见的媒体格式类型如下：
//	    text/html ： HTML格式
//	    text/plain ：纯文本格式      
//	    text/xml ：  XML格式
//	    image/gif ：gif图片格式    
//	    image/jpeg ：jpg图片格式 
//	    image/png：png图片格式
//	   以application开头的媒体格式类型：
//	   application/xhtml+xml ：XHTML格式
//	   application/xml     ： XML数据格式
//	   application/atom+xml  ：Atom XML聚合格式    
//	   application/json    ： JSON数据格式
//	   application/pdf       ：pdf格式  
//	   application/msword  ： Word文档格式
//	   application/octet-stream ： 二进制流数据（如常见的文件下载）
//	   application/x-www-form-urlencoded ： <form encType=””>中默认的encType，form表单数据被编码为key/value格式发送到服务器（表单默认的提交数据的格式）
//	   另外一种常见的媒体格式是上传文件之时使用的：
//	    multipart/form-data ： 需要在表单中进行文件上传时，就需要使用该格式
//	     以上就是我们在日常的开发中，经常会用到的若干content-type的内容格式。
	public static final String TEXT_HTML = "text/html";
	public static final String TEXT_PLAIN = "text/plain";
	public static final String TEXT_XML = "text/xml";
	public static final String TEXT_GIF = "text/gif";
	public static final String TEXT_JPEG = "text/jpeg";
	public static final String TEXT_PNG = "text/png";
	public static final String APPLICATION_XHTML_XML = "application/xhtml+xml";
	public static final String APPLICATION_XML = "application/xml";
	public static final String APPLICATION_ATOM_XML = "application/atom+xml";
	public static final String APPLICATION_JSON = "application/json";
	public static final String APPLICATION_PDF = "application/pdf";
	public static final String APPLICATION_MS_WORD = "application/msword";
	public static final String APPLICATION_OCTET_STREAM = "application/octet-stream";
	public static final String APPLICATION_URL_ENCODED = "application/x-www-form-urlencoded";
	public static final String MULTI_PART_FORM_DATA = "multipart/form-data";
	
	// 后缀相关
	public static final String HTML = ".html";
	public static final String JAVA = ".java";
	public static final String SCALA = ".scala";
	public static final String PYTHON = ".py";
	// add at 2016.05.13
	public static final String C_HEADER = ".h";
	public static final String C_SOURCE = ".c";
	public static final String CPP = ".cpp";
	public static final String PHP = ".php";
	public static final String TXT = ".txt";
	public static final String PNG = ".png";
	public static final String JPG = ".jpg";
	public static final String JPEG = ".jpeg";
	public static final String JS = ".js";
	public static final String MAP = ".map";
	public static final String ZIP = ".zip";
	public static final String IDX = ".idx";
	public static final String FIV = ".fiv";
	public static final String MP4 = ".mp4";
	public static final String GP3 = ".3gp";
	public static final String RMVB = ".rmvb";
	public static final String RM = ".rm";
	public static final String AVI = ".avi";
	public static final String LOG = ".log";
	// add at 2016.06.28
	public static final String CONF = ".conf";
	// add at 2016.05.13
	public static final String CLASS = ".class";
	public static final String DOC = ".doc";
	public static final String DOCX = ".docx";
	public static final String XLS = ".xls";
	public static final String XLSX = ".xlsx";
	public static final String PPT = ".ppt";
	public static final String PPTX = ".pptx";
	
	// 编码相关		add at 2016.04.16
	public static final String ASCII = "ascii";
	public static final String ISO_8859_1 = "iso-8859-1";
	public static final String UTF_8 = "utf-8";
	public static final String UTF_16 = "utf-16";
	public static final String GBK = "gbk";
	public static final String GB2312 = "gb2312";
	
	// 字节的表示相关
	public static final String BYTE = "byte";
	public static final String KB = "kb";
	public static final String MB = "mb";
	public static final String GB = "gb";
	public static final String TB = "tb";
	public static final String PB = "pb";
	public static final String EB = "eb";
	public static final String ZB = "zb";
	public static final String YB = "yb";
	
//	// 打印日志相关 [add at 2016.03.17]
//	public static final long LOG_ON_SAVE = 1 ;
//	public static final long LOG_ON_APPEND = LOG_ON_SAVE << 1 ;
//	public static final long LOG_ON_DELETE = LOG_ON_APPEND << 1 ;
//	public static final long LOG_ON_COPY = LOG_ON_DELETE << 1 ;
//	public static final long LOG_ON_DOWNLOAD = LOG_ON_COPY << 1 ;
//	public static final long LOG_ON_AWAIT_TASK_END = LOG_ON_DOWNLOAD << 1 ;
//	public static final long LOG_ON_FLUSH_BUFFER = LOG_ON_AWAIT_TASK_END << 1 ;
//	public static final long LOG_ON_ALL = LOG_ON_SAVE | LOG_ON_APPEND | LOG_ON_DELETE | LOG_ON_COPY 
//								| LOG_ON_DOWNLOAD | LOG_ON_AWAIT_TASK_END | LOG_ON_FLUSH_BUFFER;
//	public static final long LOG_ON_NONE = ~LOG_ON_ALL;
//	public static long LOG_ON_MINE_CONF = LOG_ON_ALL;
	
	// --------------------------- 可配置变量 --------------------------------------
	// 线程池相关
	public static int CHECK_INTERVAL = Constants.optInt(ToolsConstants._CHECK_INTERVAL);
	public static int N_THREADS = Constants.optInt(ToolsConstants._N_THREADS);
	public static ScheduledThreadPoolExecutor threadPool = new ScheduledThreadPoolExecutor(N_THREADS);
	
	// 临时文件相关
//	public static String TMP_NAME = Constants.optString(Constants._TMP_NAME);
//	public static String TMP_DIR = Constants.optString(Constants._TMP_DIR);
//	public static AtomicInteger TMP_IDX = new AtomicInteger(0);
//	public static String SUFFIX = Constants.optString(Constants._SUFFIX);
	public static final TmpGetter TMP_GETTER = new TmpGetter(Constants.optString(ToolsConstants._TMP_DIR), Constants.optString(ToolsConstants._TMP_NAME),
																0, Constants.optString(ToolsConstants._SUFFIX) );
	
	public static String DEFAULT_CHARSET = Constants.DEFAULT_CHARSET;
	public static int BUFF_SIZE_ON_TRANS_STREAM = Constants.optInt(ToolsConstants._BUFF_SIZE);
	public static int ESTIMATE_FILE_LINES = Constants.optInt(ToolsConstants._ESTIMATE_FILE_LINES);
	public static boolean WRITE_ASYNC = Constants.optBoolean(ToolsConstants._WRITE_ASYNC);
	public static boolean IS_DEBUG_ON = Constants.optBoolean(ToolsConstants._IS_DEBUG_ON);
	
	// 文件名后面可能出现的其他符号
	public static Set<Character> MAYBE_FILE_NAME_SEPS = Constants.MAYBE_FILE_NAME_SEPS;
	// 如果字符串为一下字符串, 将其视为空字符串
	public static Set<String> EMPTY_STR_CONDITIONS = Constants.EMPTY_STR_CONDITIONS;
	// ----------------- 属性结束 -----------------------
	
	// --------------------------- 配置可配置变量的接口 ----------------------------------------
	public static void setTmpIdx(int idx) {
		TMP_GETTER.setTmpIdx(idx);
	}
	public static void setTmpDir(String tmpDir) {
    	Tools.assert0(tmpDir != null, "'tmpDir' can't be null ");
    	TMP_GETTER.setTmpDir(tmpDir);
	}
	public static void setTmpName(String tmpName) {
    	Tools.assert0(tmpName != null, "'tmpName' can't be null ");
		TMP_GETTER.setTmpName(tmpName);
	}
	public static void setSuffix(String suffix) {
    	Tools.assert0(suffix != null, "'suffix' can't be null ");
		TMP_GETTER.setSuffix(suffix);
	}
	// 配置defaultCharSet
	public static void setDefaultCharSet(String defaultCharSet) {
    	Tools.assert0(defaultCharSet != null, "'defaultCharSet' can't be null ");
		DEFAULT_CHARSET = defaultCharSet;
	}
//	public static void setLogOnMine(long logOnMine) {
//		LOG_ON_MINE_CONF = logOnMine;
//	}
	public static void setBuffSize(int buffSize) {
    	Tools.assert0(buffSize > 0, "buffSize must > 0 ");
		BUFF_SIZE_ON_TRANS_STREAM = buffSize;
	}
    // 配置checkInterval
    public static void setCheckInterval(int checkInterval) {
    	Tools.assert0(checkInterval > 0, "checkInterval must > 0 ");
    	CHECK_INTERVAL = checkInterval;
    }
    // 配置线程池中线程的个数
    public static void setNThread(int nThread) {
    	Tools.assert0(nThread > 0, "nThread must > 0 ");
    	if(isThreadPoolRunning(threadPool) ) {
    		Log.err("the threadPool is running NOW, please try again later !");
    		return ;
    	}
    	
    	N_THREADS = nThread;
    	threadPool = new ScheduledThreadPoolExecutor(N_THREADS);
    }
    
	// ---------------临时文件相关---------------
	// 获取临时路径的下一个路径[返回文件路径]
	public static String getNextTmpPath() {
		return TMP_GETTER.getNextTmpPath();
	}
	public static String getNextTmpPath(String suffix) {
		return TMP_GETTER.getNextTmpPath(suffix);
	}
	public static String getTmpPath(int idx) {
		return TMP_GETTER.getTmpPath(idx);
	}
	public static String getTmpPath(int idx, String suffix) {
		return TMP_GETTER.getTmpPath(idx, suffix);
	}
	public static String getTmpPath(String name) {
		return TMP_GETTER.getTmpPath(name);
	}
	public static String getTmpPath(String name, String suffix) {
		return TMP_GETTER.getTmpPath(name, suffix);
	}
	public static String getNextTmpDir() {
		return TMP_GETTER.getNextTmpDir();
	}
	public static String getTmpDir(int idx) {
		return TMP_GETTER.getTmpDir(idx);
	}
	public static String getTmpDir(String name) {
		return TMP_GETTER.getTmpDir(name);
	}
	public static String getFilePath(String dir, String file) {
		Tools.assert0(dir != null, "'dir' can't be null ");
		Tools.assert0(file != null, "'file' can't be null ");
		return Tools.removeIfEndsWith(dir, "/") + Tools.addIfNotStartsWith(file, "/");
	}
	
	// ----------------- 文件操作相关方法 -----------------------
	// 判断是否需要打印日志
	public static boolean isLog(long logFlags, long logMask) {
		return ((logFlags & logMask) != 0);
	}
	// 将html字符串保存到指定的文件中
	// add 'isAsync' at 2016.04.16
	public static void save(String html, File targetFile, String charset, boolean isAsync) throws IOException {
		FileUtils.save(html, targetFile, charset, isAsync);
	}
	public static void save(String html, File nextTmpFile, String charset) throws IOException {
		FileUtils.save(html, nextTmpFile, charset);
	}
	public static void save(String html, File nextTmpFile, boolean isAsync) throws IOException {
		FileUtils.save(html, nextTmpFile, isAsync);
	}
	public static void save(String html, File nextTmpFile) throws IOException {
		FileUtils.save(html, nextTmpFile);
	}
	public static void save(String html, String nextTmpName, String charset, boolean isAsync) throws IOException {
		FileUtils.save(html, nextTmpName, charset, isAsync);
	}
	public static void save(String html, String nextTmpName, String charset) throws IOException {
		FileUtils.save(html, nextTmpName, charset);
	}
	public static void save(String html, String nextTmpName, boolean isAsync) throws IOException {
		FileUtils.save(html, nextTmpName, isAsync);
	}
	public static void save(String html, String nextTmpName) throws IOException {
		FileUtils.save(html, nextTmpName);
	}
	
	public static void append(String html, File nextTmpFile, String charset, boolean isAsync) throws IOException {
		FileUtils.append(html, nextTmpFile, charset, isAsync);
	}
	public static void append(String html, File nextTmpFile, String charset) throws IOException {
		FileUtils.append(html, nextTmpFile, charset);
	}
	public static void append(String html, File nextTmpFile, boolean isAsync) throws IOException {
		FileUtils.append(html, nextTmpFile, isAsync);
	}
	public static void append(String html, File nextTmpFile) throws IOException {
		FileUtils.append(html, nextTmpFile);
	}
	public static void append(String html, String nextTmpName, String charset, boolean isAsync) throws IOException {
		FileUtils.append(html, nextTmpName, charset, isAsync);
	}
	public static void append(String html, String nextTmpName, String charset) throws IOException {
		FileUtils.append(html, nextTmpName, charset);
	}	
	public static void append(String html, String nextTmpName, boolean isAsync) throws IOException {
		FileUtils.append(html, nextTmpName, isAsync);
	}
	public static void append(String html, String nextTmpName) throws IOException {
		FileUtils.append(html, nextTmpName);
	}
	
	// 1. could use 'tryWithResource' replace 'tryFinally'
	// 2. update 'BufferedOutputStream' with 'FileOutputStream' cause there need not 'Buffer'
	// at 2016.04.16
	public static void write(final String html, final File targetFile, final String charset, boolean isAsync, final boolean isAppend) throws IOException {
		FileUtils.write(html, targetFile, charset, isAsync, isAppend);
	}
	public static void write(final String html, final File targetFile, final String charset, final boolean isAppend) throws IOException {
		FileUtils.write(html, targetFile, charset, isAppend);
	}
	public static void write(final String html, final File nextTmpFile, final boolean isAppend) throws IOException {
		FileUtils.write(html, nextTmpFile, isAppend);
	}
	
	// 移除指定的文件
	public static void delete(String path) {
		FileUtils.delete(path);
	}
	
    // 复制指定的文件
    public static void copy(String src, String dst) throws IOException {
    	FileUtils.copy(src, dst);
    }

	// 获取给定的输入流中的字符内容
	public static String getContent(InputStream is, String charset) throws IOException {
		return FileUtils.getContent(is, charset);
	}
	public static String getContent(InputStream is) throws IOException {
		return FileUtils.getContent(is);
	}
	public static String getContent(String path, String charset) throws IOException {
		return FileUtils.getContent(path, charset);
	}
	public static String getContent(File file, String charset) throws IOException {
		return FileUtils.getContent(file, charset);
	}
	public static String getContent(String path) throws IOException {
		return FileUtils.getContent(path);
	}
	public static String getContent(File file) throws IOException {
		return FileUtils.getContent(file);
	}
	
	// 获取文件的所有的行, 存储在一个结果的List, 文件过大, 慎用此方法
	public static List<String> getContentWithList(File file, String charset, int estimateSize) throws IOException {
		return FileUtils.getContentWithList(file, charset, estimateSize);
	}
	public static List<String> getContentWithList(File file, int estimateSize) throws IOException {
		return FileUtils.getContentWithList(file, estimateSize);
	}
	public static List<String> getContentWithList(String file, String charset, int estimateSize) throws IOException {
		return FileUtils.getContentWithList(file, charset, estimateSize);
	}
	public static List<String> getContentWithList(String file, int estimateSize) throws IOException {
		return FileUtils.getContentWithList(file, estimateSize);
	}
	public static List<String> getContentWithList(File file, String charset) throws IOException {
		return FileUtils.getContentWithList(file, charset);
	}
	public static List<String> getContentWithList(File file) throws IOException {
		return FileUtils.getContentWithList(file);
	}
	public static List<String> getContentWithList(String file, String charset) throws IOException {
		return FileUtils.getContentWithList(file, charset);
	}
	public static List<String> getContentWithList(String file) throws IOException {
		return FileUtils.getContentWithList(file);
	}
	
	// 从指定的url上面下载图片  保存到指定的路径下面 [也适用于下载其他的二进制数据]
	public static void downloadFrom(String urlStr, String path) throws IOException {
		FileUtils.downloadFrom(urlStr, path);
	}
	
	// 将输入流中的数据 复制到输出流
	public static void copy(InputStream is, OutputStream os, boolean isCloseStream) {
		FileUtils.copy(is, os, isCloseStream);
	}
	public static void copy(InputStream is, OutputStream os) {
		FileUtils.copy(is, os);
	}
	
	// 获取指定路径的文件的文件, 通过sep分割的文件名     获取文件名
	// 解析? 的位置, 是为了防止一下情况
	public static String getFileName(String path, char sep) {
		return FileUtils.getFileName(path, sep);
	}

	// ----------------- 业务方法 -----------------------
	// 处理价格, 也可以用于处理提取字符串中的BigDecimal的情况
	public static BigDecimal dealPrice(String str) {
		return BizUtils.dealPrice(str);
	}
	
	// 处理页数, 也可以用于处理提取字符串中的整数的情况
	public static Integer dealPageNum(String str) {
		return BizUtils.dealPageNum(str);
	}
	
	// 获取站点的首页url
	// http://www.baidu.com/tieba/java/page01.jsp  =>  http://www.baidu.com/
	public static String getSiteUrl(String url) {
		return BizUtils.getSiteUrl(url);
	}
	
	// 将绝对/ 相对的url转换为绝对的url
	// 转换 /path & ./path
	public static String transformUrl(String siteUrl, String relativePath) {
		return BizUtils.transformUrl(siteUrl, relativePath);
	}

	// 如果给定的字符串以startsWith, 则移除startsWith
	public static String removeIfStartsWith(String str, String startsWith) {
		return StringUtils.removeIfStartsWith(str, startsWith);
	}
	public static String removeIfEndsWith(String str, String endsWith) {
		return StringUtils.removeIfEndsWith(str, endsWith);
	}
	public static String addIfNotStartsWith(String str, String startsWith) {
		return StringUtils.addIfNotStartsWith(str, startsWith);
	}
	public static String addIfNotEndsWith(String str, String endsWith) {
		return StringUtils.addIfNotEndsWith(str, endsWith);
	}
	
	// 判断字符串是否为空[null, "", "null"]
	public static boolean isEmpty(String str) {
		return StringUtils.isEmpty(str);
	}
	public static <T> boolean isEmpty(Collection<T> arr) {
		return CollectionUtils.isEmpty(arr);
	}
	public static <K, V> boolean isEmpty(Map<K, V> map) {
		return CollectionUtils.isEmpty(map);
	}
	// add at 2016.06.21
	public static <K, V> boolean isJSONEmpty(JSONObject obj) {
		return JSONUtils.isJSONEmpty(obj);
	}
	// add at 2016.06.02
	public static <T> boolean isEmpty(T[] arr) {
		return CollectionUtils.isEmpty(arr);
	}
	public static boolean isEmpty(int[] arr) {
		return CollectionUtils.isEmpty(arr);
	}
	public static boolean isEmpty(long[] arr) {
		return CollectionUtils.isEmpty(arr);
	}
	public static boolean isEmpty(boolean[] arr) {
		return CollectionUtils.isEmpty(arr);
	}
	public static boolean isEmpty(double[] arr) {
		return CollectionUtils.isEmpty(arr);
	}
	
	// 获取str中以start 和end之间的字符串
	public static String getStrInRange(String str, String start, String end) {
		return StringUtils.getStrInRange(str, start, end);
	}
	public static String getStrInRangeInclude(String str, String start, String end) {
		return StringUtils.getStrInRangeInclude(str, start, end);
	}
	public static String getStrInRangeWithStart(String str, String start) {
		return StringUtils.getStrInRangeWithStart(str, start);
	}
	public static String getStrInRangeWithStartInclude(String str, String start) {
		return StringUtils.getStrInRangeWithStartInclude(str, start);
	}
	public static String getStrInRangeWithEnd(String str, String end) {
		return StringUtils.getStrInRangeWithEnd(str, end);
	}
	public static String getStrInRangeWithEndInclude(String str, String end) {
		return StringUtils.getStrInRangeWithEndInclude(str, end);
	}
	public static String getStrInRange(String str, String start, String end, boolean includeStart, boolean includeEnd) {
		return StringUtils.getStrInRange(str, start, end, includeStart, includeEnd);
	}
	public static String getStrInRangeWithStart(String str, String start, boolean include) {
		return StringUtils.getStrInRangeWithStart(str, start, include);
	}
	public static String getStrInRangeWithEnd(String str, String end, boolean include) {
		return StringUtils.getStrInRangeWithEnd(str, end, include);
	}

	
	// 执行给定的任务
	public static void execute(Runnable runnable) {
		threadPool.execute(runnable);
	}

	public static ScheduledFuture<?> schedule(Runnable command, long delay, TimeUnit unit) {
		return threadPool.schedule(command, delay, unit);
	}

	public static <V> ScheduledFuture<V> schedule(Callable<V> callable, long delay, TimeUnit unit) {
		return threadPool.schedule(callable, delay, unit);
	}

	public static ScheduledFuture<?> scheduleAtFixedRate(Runnable command, long initialDelay, long period, TimeUnit unit) {
		return threadPool.scheduleAtFixedRate(command, initialDelay, period, unit);
	}

	public static ScheduledFuture<?> scheduleWithFixedDelay(Runnable command, long initialDelay, long delay, TimeUnit unit) {
		return threadPool.scheduleWithFixedDelay(command, initialDelay, delay, unit);
	}

	// 为nextStageParams添加category
	public static void addNameUrlSite(JSONObject category, JSONObject nextStageParams) {
		BizUtils.addNameUrlSite(category, nextStageParams);
	}
	
	// 通过产品的数目, 以及每一页显示的产品的数目, 计算页数
	public static int calcPageNums(int productNum, int numPerPage) {
		return BizUtils.calcPageNums(productNum, numPerPage);
	}
	
	// 将字符串的多个连续的空格转换为一个空格
	public static String trimSpacesAsOne(String str) {
		return StringUtils.trimSpacesAsOne(str);
	}
	public static String[] trimSpacesAsOne(String[] arr) {
		return StringUtils.trimSpacesAsOne(arr);
	}
	public static List<String> trimSpacesAsOne(List<String> arr) {
		return StringUtils.trimSpacesAsOne(arr);
	}

	public static String trimAllSpaces(String str, Map<Character, Character> escapeMap) {
		return StringUtils.trimAllSpaces(str, escapeMap);
	}
	public static String trimAllSpaces(String str) {
		return StringUtils.trimAllSpaces(str);
	}
	public static String[] trimAllSpaces(String[] arr, Map<Character, Character> escapeMap) {
		return StringUtils.trimAllSpaces(arr, escapeMap);
	}
	public static String[] trimAllSpaces(String[] arr) {
		return StringUtils.trimAllSpaces(arr);
	}
	public static List<String> trimAllSpaces(List<String> arr, Map<Character, Character> escapeMap) {
		return StringUtils.trimAllSpaces(arr, escapeMap);
	}
	public static List<String> trimAllSpaces(List<String> arr) {
		return StringUtils.trimAllSpaces(arr);
	}
	
	// 去掉掉obj中所有的字符串类的值的相邻的多个空格
	public static void trimSpaces(JSONObject obj) {
		JSONUtils.trimSpaces(obj);
	}
	// 去掉掉arr中所有的字符串类的值的相邻的多个空格
	public static void trimSpaces(JSONArray arr) {
		JSONUtils.trimSpaces(arr);
	}
	
	// 确保arr中的每一个JSONObject都存在指定的key, 否则  则删除该条目
	// val.toString可以确保值为null的情形
	public static void removeIfNull(JSONArray arr, String key) {
		JSONUtils.removeIfNull(arr, key);
	}
	// 去掉掉obj中所有的字符串类的值的相邻的多个空格
	public static void removeIfNull(JSONObject obj) {
		JSONUtils.removeIfNull(obj);
	}
	// 去掉掉arr中所有的字符串类的值的相邻的多个空格
	public static void removeIfNull(JSONArray arr) {
		JSONUtils.removeIfNull(arr);
	}
	
	// 从spec中获取需要的数据
	// 注意 : 必需确保spec中每一个对象为JSONObject, name为spec的数据中需要检测的值, value为spec的数据中需要获取的值, getInSpec存放获取数据的键的(key[src源对象] -> key[dst目标对象])映射
	public static void getNeededFrom(JSONArray spec, JSONObject product, String name, String value, Map<String, String> getInSpec) {
		BizUtils.getNeededFrom(spec, product, name, value, getInSpec);
	}

	// 过滤掉不需要的字符
	public static String filter(String str, Set<Character> needBeFiltered) {
		return StringUtils.filter(str, needBeFiltered);
	}
	public static JSONObject filter(JSONObject obj, Set<String> needBeFiltered) {
		return JSONUtils.filter(obj, needBeFiltered);
	}
	
	// 向sb中添加str
	public static void append(StringBuilder sb, String str, boolean isClean) {
		StringUtils.append(sb, str, isClean);
	}
	public static void append(StringBuilder sb, String str) {
		StringUtils.append(sb, str);
	}
	public static void appendCRLF(StringBuilder sb, String str, boolean isClean) {
		StringUtils.appendCRLF(sb, str, isClean);
	}
	public static void appendCRLF(StringBuilder sb, String str) {
		StringUtils.appendCRLF(sb, str);
	}
	
	// 获取键值对类型的数据对, 添加到headers中
	public static void addHeaders(File configFile, Map<String, String> headers, String sep) throws IOException {
		BizUtils.addHeaders(configFile, headers, sep);
	}
	
	// 解码含有unicode字符串的字符串
	// 遍历一次字符串, 寻找出匹配"\\uxxxx"的字符串, 然后将其解码为字符[unicode -> char]
	// 对于其他的字符不作处理
	public static String unicodeDecode(String str) {
		return BizUtils.unicodeDecode(str);
	}
	// 判断给定的字符是否可表示十六进制[0-9, a-f, A-F]
	public static boolean isHexChar(char ch) {
		return BizUtils.isHexChar(ch);
	}
	
	
	// ------------ 线程池相关 --------------------
	// awaitTermination 线程池
//	public static void awaitTermination(long timout, TimeUnit unit) {
//		try {
//			threadPool.awaitTermination(timout, unit);
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}
//	}
	
	// 添加一个任务
	public static void addTask(Runnable run) {
		threadPool.execute(run);
	}
    // 新建一个线程池
    public static ThreadPoolExecutor newFixedThreadPool(int nThread) {
    	Tools.assert0(nThread > 0, "nThread must > 0 ");
    	return (ThreadPoolExecutor) Executors.newFixedThreadPool(nThread);
    }
    
	// shutdown 线程池
	public static void awaitShutdown(ThreadPoolExecutor threadPool, int checkInterval) {
		awaitTasksEnd(threadPool, checkInterval, true);
	}
	public static void awaitShutdown() {
		awaitShutdown(threadPool, CHECK_INTERVAL);
	}
	
    // 等待 线程池中任务结束 [并不关闭线程池]
    public static void awaitTasksEnd(ThreadPoolExecutor threadPool, int checkInterval) {
    	awaitTasksEnd(threadPool, checkInterval, false);
    }
    public static void awaitTasksEnd() {
    	awaitTasksEnd(threadPool, CHECK_INTERVAL, false);
    }
    public static void awaitTasksEnd(ThreadPoolExecutor threadPool, int checkInterval, boolean isShutdown) {
    	Tools.assert0(threadPool != null, "'threadPool' can't be null ");
    	Tools.assert0(checkInterval > 0, "'checkInterval' must > 0 ");
    	
        while (! threadPool.isShutdown() ) {
        	int taskInQueue = threadPool.getQueue().size();
        	int activeTaskCount = threadPool.getActiveCount();
            if((taskInQueue == 0) && (activeTaskCount == 0) ) {
            	if(isShutdown) {
            		threadPool.shutdown();
            	}
                break ;
            } else {
                Tools.sleep(checkInterval);
            }
        }
    }
    // 判断给定的线程池是否还有任务在运行
    public static boolean isThreadPoolRunning(ScheduledThreadPoolExecutor threadPool) {
    	Tools.assert0(threadPool != null, "'threadPool' can't be null ");
    	int taskInQueue = threadPool.getQueue().size();
    	int activeTaskCount = threadPool.getActiveCount();
    	return ((taskInQueue != 0) || (activeTaskCount != 0) );
    }
    // 为threadPoolExecutor重新分配实例
    public static void reallocateThreadPoolExecutor() {
    	setNThread(N_THREADS);
    }

	// 让当前线程休息ms
	public static void sleep(long ms) {
		try {
			Thread.sleep(ms);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
	
	// 获取现在的毫秒数, 以及根据start获取开销的时间
	public static long now() {
		return DateUtils.now();
	}
	public static String nowStr() {
		return DateUtils.nowStr();
	}
	public static String formatedNowStr() {
		return DateUtils.formatedNowStr();
	}
	public static long spent(long start) {
		return DateUtils.spent(start);
	}
	public static String spentStr(long start) {
		return DateUtils.spentStr(start);
	}
	
	// ------------ 进制转换相关 --------------------
	
    // 根据长度, 获取长度的字符串表示
	public static String getLengthString(long length, String dimen) {
		return BizUtils.getLengthString(length, dimen);
	}
	public static String getLengthString(long length, String dimen, GetLengthStrMethod getLengthStrMethod) {
		return BizUtils.getLengthString(length, dimen, getLengthStrMethod);
	}
	
	// 根据字节数, 获取千字节数, 兆字节数, 吉字节数, 踢字节数
	public static long getKBytesByBytes(long bytes) {
		return BizUtils.getKBytesByBytes(bytes);
	}
	public static long getMBytesByBytes(long bytes) {
		return BizUtils.getMBytesByBytes(bytes);
	}
	public static long getGBytesByBytes(long bytes) {
		return BizUtils.getGBytesByBytes(bytes);
	}
	public static long getTBytesByBytes(long bytes) {
		return BizUtils.getTBytesByBytes(bytes);
	}
	public static long getPBytesByBytes(long bytes) {
		return BizUtils.getPBytesByBytes(bytes);
	}
	public static long getEBytesByBytes(long bytes) {
		return BizUtils.getEBytesByBytes(bytes);
	}
	public static long getZBytesByBytes(long bytes) {
		return BizUtils.getZBytesByBytes(bytes);
	}
	public static long getYBytesByBytes(long bytes) {
		return BizUtils.getYBytesByBytes(bytes);
	}
	
	// ------------ 缓冲相关 ------- 2016.03.16 -------------
	public static final BufferManager BUFFER_MANAGER = new BufferManager();
	
	// 获取所有的缓冲区的key的集合
	public static Set<String> buffNames() {
		return BUFFER_MANAGER.buffNames();
	}
	// 创建一个缓冲区
	public static void createAnBuffer(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
		BUFFER_MANAGER.createAnBuffer(bufName, outputPath, charset, threshold, buffSizeEstimator, handler);
	}
	public static void createAnBuffer(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
		BUFFER_MANAGER.createAnBuffer(bufName, outputPath, charset, threshold, buffSizeEstimator);
	}
	public static void createAnBuffer(String bufName, String outputPath, String charset) {
		BUFFER_MANAGER.createAnBuffer(bufName, outputPath, charset);
	}
	public static void createAnBuffer(String bufName, String outputPath) {
		BUFFER_MANAGER.createAnBuffer(bufName, outputPath);
	}
	public static void createAnBufferIfNotExists(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
		BUFFER_MANAGER.createAnBufferIfNotExists(bufName, outputPath, charset, threshold, buffSizeEstimator, handler);
	}
	public static void createAnBufferIfNotExists(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
		BUFFER_MANAGER.createAnBufferIfNotExists(bufName, outputPath, charset, threshold, buffSizeEstimator);
	}
	public static void createAnBufferIfNotExists(String bufName, String outputPath, String charset, BufferHandler handler) {
		BUFFER_MANAGER.createAnBufferIfNotExists(bufName, outputPath, charset, handler);
	}
	public static void createAnBufferIfNotExists(String bufName, String outputPath, String charset) {
		BUFFER_MANAGER.createAnBufferIfNotExists(bufName, outputPath, charset);
	}
	public static void createAnBufferIfNotExists(String bufName, String outputPath) {
		BUFFER_MANAGER.createAnBufferIfNotExists(bufName, outputPath);
	}
	public static void closeAnBuffer(String bufName) throws Exception {
		BUFFER_MANAGER.closeAnBuffer(bufName);
	}
	public static void closeAllBuffer() throws Exception {
		BUFFER_MANAGER.closeAllBuffer();
	}
	// 判断给定的bufName的buffer是否存在
	public static boolean bufExists(String buffName) {
		return BUFFER_MANAGER.bufExists(buffName);
	}
	public static BufferManager.BuffInfo getBuffInfo(String buffName) {
		return BUFFER_MANAGER.getBuffInfo(buffName);
	}
	
	// 向给定的缓冲区中添加数据 并检测buffer中的数据是否超过了阈值
	public static void appendBuffer(String bufName, String content, boolean appendCRLF) throws Exception {
		BUFFER_MANAGER.appendBuffer(bufName, content, appendCRLF);
	}
	public static void appendBuffer(String bufName, String content) throws Exception {
		BUFFER_MANAGER.appendBuffer(bufName, content);
	}
	public static void appendBufferCRLF(String bufName, String content) throws Exception {
		BUFFER_MANAGER.appendBufferCRLF(bufName, content);
	}
	
	// 刷出缓存的数据
	public static void flushBuffer(String bufName, boolean isLastBatch) throws Exception {
		BUFFER_MANAGER.flushBuffer(bufName, isLastBatch);
	}
	public static void flushBuffer(String bufName) throws Exception {
		BUFFER_MANAGER.flushBuffer(bufName);
	}
	
	// update the step 'flushDataToPath' into 'threadPoolExecutor'		at 2016.04.16
	public static void flushBuffer(final StringBuffer sb, final String path, final String charset) throws IOException {
		BufferManager.flushBuffer(sb, path, charset);
	}
	public static void flushBuffer(StringBuffer sb, String path) throws IOException {
		BUFFER_MANAGER.flushBuffer(sb, path);
	}
	
	// ------------ assert相关 ------- 2016.03.22 -------------
	// 工具方法
	// 确保boo为true, 否则 抛出异常
	public static void assert0(String msg) {
		AssertUtils.assert0(msg);
	}
	public static void assert0(boolean boo, String msg) {
		AssertUtils.assert0(boo, msg);
	}
	// add at 2016.05.02
	public static void assert0(Exception e) {
		AssertUtils.assert0(e);
	}
	public static void assert0(boolean boo, Exception e) {
		AssertUtils.assert0(boo, e);
	}
	// 确保val 和expected相同, 否则 抛出异常
	public static void assert0(int val, int expect, String errorMsg) {
		AssertUtils.assert0(val, expect, errorMsg);
	}
	public static void assert0(int val, int expect, boolean isEquals, String errorMsg) {
		AssertUtils.assert0(val, expect, isEquals, errorMsg);
	}
	public static <T> void assert0(T val, T expect, String errorMsg) {
		AssertUtils.assert0(val, expect, errorMsg);
	}
	public static <T> void assert0(T val, T expect, boolean isEquals, String errorMsg) {
		AssertUtils.assert0(val, expect, isEquals, errorMsg);
	}
	// add at 2016.06.28
	// for bug correctness !
	public static void assert1(String msg) {
		if(IS_DEBUG_ON) {
			assert0(msg);
		}
	}
	public static void assert1(boolean boo, String msg) {
		if(IS_DEBUG_ON) {
			assert0(boo, msg);
		}
	}
	public static void assert1(Exception e) {
		if(IS_DEBUG_ON) {
			assert0(e);
		}
	}
	public static void assert1(boolean boo, Exception e) {
		if(IS_DEBUG_ON) {
			assert0(boo, e);
		}
	}
	// 确保val 和expected相同, 否则 抛出异常
	public static void assert1(int val, int expect, String errorMsg) {
		if(IS_DEBUG_ON) {
			assert0(val, expect, errorMsg);
		}
	}
	public static void assert1(int val, int expect, boolean isEquals, String errorMsg) {
		if(IS_DEBUG_ON) {
			assert0(val, expect, isEquals, errorMsg);
		}
	}
	public static <T> void assert1(T val, T expect, String errorMsg) {
		if(IS_DEBUG_ON) {
			assert0(val, expect, errorMsg);
		}
	}
	public static <T> void assert1(T val, T expect, boolean isEquals, String errorMsg) {
		if(IS_DEBUG_ON) {
			assert0(val, expect, isEquals, errorMsg);
		}
	}
	
	// ------------ 将数据复制到剪切板 ------- 2016.04.07 -------------
	// windows剪切板 和内存交互数据
	public static void copyStringToClipBoard(String str) {
		OsUtils.copyStringToClipBoard(str);
	}
	public static void copyImgToClipBoard(RenderedImage img) {
		OsUtils.copyImgToClipBoard(img);
    }
	public static void copyFilesToClipBoard(List<File> files) {
      OsUtils.copyFilesToClipBoard(files);
    }	
	public static String getStringFromClipBoard(){
		return OsUtils.getStringFromClipBoard();
	}
	public static RenderedImage getImgFromClipBoard() {
		return OsUtils.getImgFromClipBoard();
   }
   public static List<File> getFilesFromClipBoard() {
      return OsUtils.getFilesFromClipBoard();
   }
	
	// ------------ asList / Set / Map ------- 2016.04.24 -------------
   public static <T> List<T> asList(T... eles) {
	   return CollectionUtils.asList(eles);
   }
   public static <T> List<T> asLinkedList(T... eles) {
	   return CollectionUtils.asLinkedList(eles);
   }
   public static <T> List<T> asList(T[]... eles) {
	   return CollectionUtils.asList(eles);
   }
   public static <T> List<T> asLinkedList(T[]... eles) {
	   return CollectionUtils.asLinkedList(eles);
   }
   public static <T> List<T> asList(List<T> ls, T... eles) {
	   return CollectionUtils.asList(ls, eles);
   }
   public static <T> Set<T> asSet(T... eles) {
	   return CollectionUtils.asSet(eles);
   }
   public static <T> Set<T> asLinkedSet(T... eles) {
	   return CollectionUtils.asLinkedSet(eles);
   }
   public static <T> Set<T> asSortedSet(T... eles) {
	   return CollectionUtils.asSortedSet(eles);
   }
   public static <T> Set<T> asSet(T[]... eles) {
	   return CollectionUtils.asSet(eles);
   }
   public static <T> Set<T> asLinkedSet(T[]... eles) {
	   return CollectionUtils.asLinkedSet(eles);
   }
   public static <T> Set<T> asSortedSet(T[]... eles) {
	   return CollectionUtils.asSortedSet(eles);
   }
   public static <T> Set<T> asSet(Set<T> set, T... eles) {
	   return CollectionUtils.asSet(set, eles);
   }
   public static <K, V> Map<K, V> asMap(K key, V val) {
	   return CollectionUtils.asMap(key, val);
   }
   public static <K, V> Map<K, V> asLinkedMap(K key, V val) {
	   return CollectionUtils.asLinkedMap(key, val);
   }
   public static <K, V> Map<K, V> asSortedMap(K key, V val) {
	   return CollectionUtils.asSortedMap(key, val);
   }
   public static <K, V> Map<K, V> asMap(K[] keys, V... vals) {
	   return CollectionUtils.asMap(keys, vals);
   }
   public static <K, V> Map<K, V> asLinkedMap(K[] keys, V... vals) {
	   return CollectionUtils.asLinkedMap(keys, vals);
   }
   public static <K, V> Map<K, V> asSortedMap(K[] keys, V... vals) {
	   return CollectionUtils.asSortedMap(keys, vals);
   }
   public static <K, V> Map<K, V> asMap(Map<K, V> map, K[] keys, V... vals) {
	   return CollectionUtils.asMap(map, keys, vals);
   }
   
   
   // add at 2016.05.07
   
   // 获取给定的JSONObject的给定的索引的数据
   // with 'defaultValue'
   public static String getString(Map<String, Object> map, int idx, String[] idxes) {
       return MapUtils.getString(map, idx, idxes);
   }
   public static String optString(Map<String, Object> map, int idx, String[] idxes) {
       return MapUtils.optString(map, idx, idxes);
   }
   public static String optString(Map<String, Object> map, int idx, String[] idxes, String defaultValue) {
	   return MapUtils.optString(map, idx, idxes, defaultValue);
   }
   public static int getInt(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getInt(map, idx, idxes);
   }
   public static int optInt(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optInt(map, idx, idxes);
   }
   public static int optInt(Map<String, Object> map, int idx, String[] idxes, int defaultValue) {
	   return MapUtils.optInt(map, idx, idxes, defaultValue);
   }
   public static boolean getBoolean(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getBoolean(map, idx, idxes);
   }
   public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optBoolean(map, idx, idxes);
   }
   public static boolean optBoolean(Map<String, Object> map, int idx, String[] idxes, boolean defaultValue) {
	   return MapUtils.optBoolean(map, idx, idxes, defaultValue);
   }
   public static long getLong(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getLong(map, idx, idxes);
   }
   public static long optLong(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optLong(map, idx, idxes);
   }
   public static long optLong(Map<String, Object> map, int idx, String[] idxes, long defaultValue) {
	   return MapUtils.optLong(map, idx, idxes, defaultValue);
   }
   public static double getDouble(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getDouble(map, idx, idxes);
   }
   public static double optDouble(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optDouble(map, idx, idxes);
   }
   public static double optDouble(Map<String, Object> map, int idx, String[] idxes, double defaultValue) {
	   return MapUtils.optDouble(map, idx, idxes, defaultValue);
   }
   public static JSONObject getJSONObject(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getJSONObject(map, idx, idxes);
   }
   public static JSONObject optJSONObject(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optJSONObject(map, idx, idxes);
   }
   public static JSONArray getJSONArray(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.getJSONArray(map, idx, idxes);
   }
   public static JSONArray optJSONArray(Map<String, Object> map, int idx, String[] idxes) {
	   return MapUtils.optJSONArray(map, idx, idxes);
   }
   
   // with 'defaultIdx'
   public static String getString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getString(map, idx, defaultIdx, idxes);
   }
   public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optString(map, idx, defaultIdx, idxes);
   }
   public static String optString(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, String defaultValue) {
	   return MapUtils.optString(map, idx, defaultIdx, idxes, defaultValue);
   }
   public static int getInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getInt(map, idx, defaultIdx, idxes);
   }
   public static int optInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optInt(map, idx, defaultIdx, idxes);
   }
   public static int optInt(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, int defaultValue) {
	   return MapUtils.optInt(map, idx, defaultIdx, idxes, defaultValue);
   }
   public static long getLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getLong(map, idx, defaultIdx, idxes);
   }
   public static long optLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optLong(map, idx, defaultIdx, idxes);
   }
   public static long optLong(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, long defaultValue) {
	   return MapUtils.optLong(map, idx, defaultIdx, idxes, defaultValue);
   }
   public static double getDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getDouble(map, idx, defaultIdx, idxes);
   }
   public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optDouble(map, idx, defaultIdx, idxes);
   }
   public static double optDouble(Map<String, Object> map, int idx, int defaultIdx, String[] idxes, double defaultValue) {
	   return MapUtils.optDouble(map, idx, defaultIdx, idxes, defaultValue);
   }
   public static JSONObject getJSONObject(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getJSONObject(map, idx, defaultIdx, idxes);
   }
   public static JSONObject optJSONObject(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optJSONObject(map, idx, defaultIdx, idxes);
   }
   public static JSONArray getJSONOArray(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.getJSONOArray(map, idx, defaultIdx, idxes);
   }
   public static JSONArray optJSONArray(Map<String, Object> map, int idx, int defaultIdx, String[] idxes) {
	   return MapUtils.optJSONArray(map, idx, defaultIdx, idxes);
   }
   
   // add getString / Int / ...(List, int) 		at 2016.06.02		
   public static String getString(List arr, int idx) {
	   return MapUtils.getString(arr, idx);
   }
   public static String optString(List arr, int idx, String defaultValue) {
	   return MapUtils.optString(arr, idx, defaultValue);
   }
   public static String optString(List arr, int idx) {
	   return MapUtils.optString(arr, idx);
   }
   public static int getInt(List arr, int idx) {
	   return MapUtils.getInt(arr, idx);
   }
   public static int optInt(List arr, int idx, int defaultValue) {
	   return MapUtils.optInt(arr, idx, defaultValue);
   }
   public static int optInt(List arr, int idx) {
	   return MapUtils.optInt(arr, idx);
   }
   public static boolean getBoolean(List arr, int idx) {
	   return MapUtils.getBoolean(arr, idx);
   }
   public static boolean optBoolean(List arr, int idx, boolean defaultValue) {
	   return MapUtils.optBoolean(arr, idx, defaultValue);
   }
   public static boolean optBoolean(List arr, int idx) {
	   return MapUtils.optBoolean(arr, idx);
   }
   public static long getLong(List arr, int idx) {
	   return MapUtils.getLong(arr, idx);
   }
   public static long optLong(List arr, int idx, long defaultValue) {
	   return MapUtils.optLong(arr, idx, defaultValue);
   }
   public static long optLong(List arr, int idx) {
	   return MapUtils.optLong(arr, idx);
   }
   public static double getDouble(List arr, int idx) {
	   return MapUtils.getDouble(arr, idx);
   }
   public static double optDouble(List arr, int idx, double defaultValue) {
	   return MapUtils.optDouble(arr, idx, defaultValue);
   }
   public static double optDouble(List arr, int idx) {
	   return MapUtils.optDouble(arr, idx);
   }
   public static JSONObject getJSONObject(List arr, int idx) {
	   return MapUtils.getJSONObject(arr, idx);
   }
   public static JSONObject optJSONObject(List arr, int idx, JSONObject defaultValue) {
	   return MapUtils.optJSONObject(arr, idx, defaultValue);
   }
   public static JSONObject optJSONObject(List arr, int idx) {
	   return MapUtils.optJSONObject(arr, idx);
   }
   public static JSONArray getJSONArray(List arr, int idx) {
	   return MapUtils.getJSONArray(arr, idx);
   }
   public static JSONArray optJSONArray(List arr, int idx, JSONArray defaultValue) {
	   return MapUtils.optJSONArray(arr, idx, defaultValue);
   }
   public static JSONArray optJSONArray(List arr, int idx) {
	   return MapUtils.optJSONArray(arr, idx);
   }
   
   // get / optString (map, key, defaultValue)
   public static String getString(Map<String, Object> map, String key) {
	   return MapUtils.getString(map, key);
   }
   public static String optString(Map<String, Object> map, String key, String defaultValue) {
	   return MapUtils.optString(map, key, defaultValue);
   }
   public static String optString(Map<String, Object> map, String key) {
	   return MapUtils.optString(map, key);
   }
   public static int getInt(Map<String, Object> map, String key) {
	   return MapUtils.getInt(map, key);
   }
   public static int optInt(Map<String, Object> map, String key, int defaultValue) {
	   return MapUtils.optInt(map, key, defaultValue);
   }
   public static int optInt(Map<String, Object> map, String key) {
	   return MapUtils.optInt(map, key);
   }
   public static boolean getBoolean(Map<String, Object> map, String key) {
	   return MapUtils.getBoolean(map, key);
   }
   public static boolean optBoolean(Map<String, Object> map, String key, boolean defaultValue) {
	   return MapUtils.optBoolean(map, key, defaultValue);
   }
   public static boolean optBoolean(Map<String, Object> map, String key) {
	   return MapUtils.optBoolean(map, key);
   }
   public static long getLong(Map<String, Object> map, String key) {
	   return MapUtils.getLong(map, key);
   }
   public static long optLong(Map<String, Object> map, String key, long defaultValue) {
	   return MapUtils.optLong(map, key, defaultValue);
   }
   public static long optLong(Map<String, Object> map, String key) {
	   return MapUtils.optLong(map, key);
   }
   public static double getDouble(Map<String, Object> map, String key) {
	   return MapUtils.getDouble(map, key);
   }
   public static double optDouble(Map<String, Object> map, String key, double defaultValue) {
	   return MapUtils.optDouble(map, key, defaultValue);
   }
   public static double optDouble(Map<String, Object> map, String key) {
	   return MapUtils.optDouble(map, key);
   }
   public static JSONObject getJSONObject(Map<String, Object> map, String key) {
	   return MapUtils.getJSONObject(map, key);
   }
   public static JSONObject optJSONObject(Map<String, Object> map, String key, JSONObject defaultValue) {
	   return MapUtils.optJSONObject(map, key, defaultValue);
   }
   public static JSONObject optJSONObject(Map<String, Object> map, String key) {
	   return MapUtils.optJSONObject(map, key);
   }
   public static JSONArray getJSONArray(Map<String, Object> map, String key) {
	   return MapUtils.getJSONArray(map, key);
   }
   public static JSONArray optJSONArray(Map<String, Object> map, String key, JSONArray defaultValue) {
	   return MapUtils.optJSONArray(map, key, defaultValue);
   }
   public static JSONArray optJSONArray(Map<String, Object> map, String key) {
	   return MapUtils.optJSONArray(map, key);
   }
   
   // 获取索引相关
   public static int getIdx(int idx, String[] idxes) {
	   return IdxUtils.getIdx(idx, idxes);
   }
   public static int getIdx(int idx, Collection<String> idxes) {
	   return IdxUtils.getIdx(idx, idxes);
   }
   public static int getIdx(int idx, int maxSize) {
	   return IdxUtils.getIdx(idx, maxSize);
   }
   public static int getIdx(int idx, String[] idxes, int defaultIdx) {
	   return IdxUtils.getIdx(idx, idxes, defaultIdx);
   }
   public static int getIdx(int idx, Collection<String> idxes, int defaultIdx) {
	   return IdxUtils.getIdx(idx, idxes, defaultIdx);
   }
   public static int getIdx(int idx, int maxSize, int defaultIdx) {
	   return IdxUtils.getIdx(idx, maxSize, defaultIdx);
   }
   
   // add at 2016.05.17
   // 增加封装get请求的查询字符串
   public static String encapQueryString(Map<String, String> params) {
	   return BizUtils.encapQueryString(params);
   }
	// 通过cookies获取cookie的字符串表示
	public static String getCookieStr(Map<String, String> cookies) {
		return BizUtils.getCookieStr(cookies);
	}
	// 通过cookie格式的字符串 获取各个cookie [这里 直接使用split, 避免出现错误]
	public static Map<String, String> getCookiesByCookieStr(String cookiesStr) {
		return BizUtils.getCookiesByCookieStr(cookiesStr);
	}
	// 移除掉sb的添加的最后一个分隔符
	public static void removeLastSep(StringBuilder sb, String lastSep) {
		StringUtils.removeLastSep(sb, lastSep);
	}
	
    // add at 2016.05.18
	// 获取标准的大写 或者小写
	public static String getStdCase(String str) {
		return StringUtils.getStdCase(str);
	}
	public static String getStdCase(String str, boolean isUpperCase) {
		return StringUtils.getStdCase(str, isUpperCase);
	}
	// 判断str01 和str02是否相同[忽略大小写]
	public static boolean equalsIgnoreCase(String str01, String str02) {
		return StringUtils.equalsIgnoreCase(str01, str02);
	}
	// 如果给定的字符串的首字母是大写的话, 将其转换为小写
	public static String lowerCaseFirstChar(String str) {
		return StringUtils.lowerCaseFirstChar(str);
	}
	public static String upperCaseFirstChar(String str) {
		return StringUtils.upperCaseFirstChar(str);
	}
	
	// 获取给定的异常的信息
	public static String errorMsg(Exception e) {
		return BizUtils.errorMsg(e);
	}
	
	// add at 2016.06.18
	// 驼峰 -> 下划线表示
	public static String camel2UnderLine(String name) {
		return BizUtils.camel2UnderLine(name);
	}
	public static String underLine2Camel(String name) {
		return BizUtils.underLine2Camel(name);
	}
	
	// add at 2016.08.25
	public static String getClazzNameByFullName(String fullName) {
		return BizUtils.getClazzNameByFullName(fullName);
	}

	// add at 2016.08.11
	// 判断给定的字符数组中是否包含给定的字符
	public static boolean contains(int[] arr, int ele) {
		return CollectionUtils.contains(arr, ele);
	}
	public static boolean contains(long[] arr, long ele) {
		return CollectionUtils.contains(arr, ele);
	}
	public static boolean contains(double[] arr, double ele) {
		return CollectionUtils.contains(arr, ele);
	}
	public static boolean contains(char[] arr, char ele) {
		return CollectionUtils.contains(arr, ele);
	}
	public static boolean contains(boolean[] arr, boolean ele) {
		return CollectionUtils.contains(arr, ele);
	}
	public static <T> boolean contains(T[] arr, T ele) {
		return CollectionUtils.contains(arr, ele);
	}
	
	/**
	 * @Name: replaceO 
	 * @Description: 替换给定的字符串为目标字符串
	 * 	为了增加HXAttrHandler.replaceO[replaceOriginal]而添加
	 * @param str
	 * @param src
	 * @param dst
	 * @return  
	 * @Create at 2016-09-30 21:51:15 by '970655147'
	 */
	public static String replaceO(String str, String src, String dst) {
		return StringUtils.replaceO(str, src, dst);
	}
	
	/**
	 * @Name: replaceO 
	 * @Description: 同时替换多个字符串
	 * 	[这里 可能会出现WordSeprator的一些问题, 因此 可以借此机会修正修正]
	 * @param str
	 * @param mapper 需要映射的字符串kv对
	 * @return  
	 * @Create at 2016-09-30 22:21:53 by '970655147'
	 */
	public static String replaceO(String str, Map<String, String> mapper) {
		return StringUtils.replaceO(str, mapper);
	}

	// add at 2016.11.23
	/**
	 * @Name: isCommentLine 
	 * @Description: 判断给定的line是否是单行注释[//, --, #, ;]
	 * @param line
	 * @return  
	 * @Create at 2016-11-23 21:51:39 by '970655147'
	 */
	public static boolean isCommentLine(String line) {
		return StringUtils.isCommentLine(line);
	}
	
	
	/**
	 * 计算封装给定size个元素, HashMap需要的容量
	 * 
	 * @Name: estimateMapSize 
	 * @Description: TODO
	 * @param size
	 * @return  
	 * @Create at 2016-12-14 20:08:33 by '970655147'
	 */
	public static int estimateMapSize(int size) {
		return MapUtils.estimateMapSize(size);
	}

    // ------------ 待续 --------------------

	
}

/**
 * file name : Test05ForLog.java
 * created at : 8:12:33 PM Apr 7, 2016
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.debug;
import static com.hx.log.util.Log.fatal;
import static com.hx.log.util.Log.info;
import static com.hx.log.util.Log.warn;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import com.hx.log.util.Constants;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

public class Test05ForLog {

	// ≤‚ ‘log ‰≥ˆ
	public static void main(String[] args) throws IOException {
		
		String str = "Hello World !";
		List<String> ls = Arrays.asList("sdf", "hel", "sdf");
		int[] intArr = {1, 3, 4, 2, 3, 5 };
		int[][] twoDimenIntArr = { 
				{1, 3, 4, 2, 3, 5 },
				{3, 2, 1, 4, 6, 6 }
			};
		boolean[] booArr = {true, false, true };
		boolean[][] twoDimenBooArr = { 
				{true, false, true },
				{false, false, true }
		};
		
		Log.log(str);
		Log.log(str, true);
		Log.err(str);
		Log.err(str, true);
		
		Log.log(ls.iterator() );
		Log.log(ls.iterator(), "--");
		Log.err(ls.iterator() );
		Log.err(ls.iterator(), "--");
		
		Log.log(ls );
		Log.log(ls, "--");
		Log.err(ls );
		Log.err(ls, "--");
		
		Log.log(intArr );
		Log.log(intArr, "--");
		Log.err(twoDimenIntArr );
		Log.err(twoDimenIntArr, "--");

		Log.log(ls );
		Log.log(ls, "--");
		Log.err(ls );
		Log.err(ls, "--");
		
		Log.log(booArr );
		Log.log(booArr, "--");
		Log.err(twoDimenBooArr );
		Log.err(twoDimenBooArr, "--");
		
		Log.logEnter();
		Log.flush();
		
		Log.log(Tools.DEFAULT_CHARSET );
		
		test();
		
//		Log.setOutLogFile(Tools.getTmpPath(222, Tools.TXT) );
		new Thread(new Runnable() {
			public void run() {
				Log.logHorizon();
			}
		}, "testThread").start();
		
		
		Tools.awaitShutdown();
		Log.flush();
		
//		Log.log("abc", true, Constants.ERR_IDX);
		Log.log("abc", true, 6);
		
//		Log.log(Log.log.logBuffNames);
//		Log.log(new Logger().logBuffNames);
//		Log.log(new Logger().logBuffNames);
//		Log.log(new Logger().logBuffNames);
		
		Log.log(Log.log.errLogPatternFormat("abc", true) );
		
		Log.log(true);
		Log.log(1);
		
		Log.logEnter(3);
		Log.dispathLogInfo(Constants.OUT_IDX, "Hello World");
		Log.dispathLogInfo(Constants.OUT_IDX, "Hello World", false);
		
		
		info("info com.hx.test");
//		infoFatalLogger.setErrStream(System.out);
		fatal("fatal com.hx.test");
	
		debug("debug com.hx.test");
		warn("warn com.hx.test");
		
	}
	
	// ≤‚ ‘${stackTrace }
	public static void test() {
		Log.log("com.hx.test ${stackTrace } ");
	}
	
}

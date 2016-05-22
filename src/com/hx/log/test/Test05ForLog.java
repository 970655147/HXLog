/**
 * file name : Test05ForLog.java
 * created at : 8:12:33 PM Apr 7, 2016
 * created by 970655147
 */

package com.hx.log.test;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import com.hx.log.log.Log;
import com.hx.log.log.Tools;

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
		
	}
	
	// ≤‚ ‘${stackTrace }
	public static void test() {
		Log.log("test ${stackTrace } ");
	}
	
}

/**
 * file name : Test06SepLogPattern.java
 * created at : 10:03:22 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.test;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;

import com.hx.log.log.Log;
import com.hx.log.util.Logger;
import com.hx.log.util.Tools;
import com.hx.log.util.WordsSeprator;

public class Test06SepLogPattern {

	// ≤‚ ‘∑÷∏ÓlogPattern
	public static void main(String[] args) throws IOException {
		
		String str = "${PREFIX } ${time } ${mode } ${msg } ~~~";
		Set<String> sepToPos = new HashSet<>();
		sepToPos.add("${");
		sepToPos.add("}");
		Tools.setDefaultCharSet(Tools.GBK);
		
//		Log.setOutToLogFile(true, Tools.getTmpPath("log1", Tools.LOG) );
		WordsSeprator sep = new WordsSeprator(str, sepToPos, null, true);
		while(sep.hasNext() ) {
			Log.log(sep.next() );
		}
		
//		Log.setLogFile(Tools.getTmpPath("log2", Tools.LOG) );
		Log.log("abc");
		
//		Log.flush();
//		Tools.awaitShutdown();
		
		new Logger().log();
		
		
	}
	
}

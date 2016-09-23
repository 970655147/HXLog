/**
 * file name : Test06SepLogPattern.java
 * created at : 10:03:22 PM Apr 21, 2016
 * created by 970655147
 */

package com.hx.log.test;

import static com.hx.log.util.Log.log;

import java.io.IOException;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import com.hx.log.util.Log;
import com.hx.log.util.LogPattern.LogPatternChain;
import com.hx.log.util.LogPatternUtils;
import com.hx.log.util.Logger;
import com.hx.log.util.Tools;
import com.hx.log.util.WordsSeprator;

import net.sf.json.JSONObject;

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
		
		
		LogPatternChain logPattern = LogPatternUtils.initLogPattern(str);
		log(LogPatternUtils.formatLogInfo(logPattern, new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]").element("msg", "got sdf")) );
		
		
		String optionalStr = "${PREFIX } ${time } ${mode } ${msg } ~~~ $[${this} ---$[${opt} ||| ]--- ]";
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("this", "$this$").element("opt", "#o#p#t#")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("this", "$this$")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf")) );
		log(LogPatternUtils.formatLogInfo(LogPatternUtils.initLogPattern(optionalStr), 
				new JSONObject().element("PREFIX", "sdf").element("time", new Date().toString()).element("mode", "[LOG]")
				.element("msg", "got sdf").element("opt", "#o#p#t#")) );
		
		
		
	}
	
}

/**
 * file name : Test03FileNameMatcherTest.java
 * created at : 5:55:22 PM Nov 29, 2015
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.util.Constants;
import com.hx.common.file.FileNameMatcher;
import com.hx.log.util.Log;

public class Test03_FileNameMatcherTest {

	// ≤‚ ‘FileNameMatcher
	public static void main(String []args) {
		
		Log.log.logPatternChain = Constants.JUST_PRINT_MSG_LOG_PATTERN;
		
//		String fileName = "setInterval(sendMsg('baoheizi'), 2000)";
//		String pattern = "set*v*(?end*";
//		String pattern = "*";
		
		
//		String fileName = "programBooter";
//		String pattern = "program*";
		
		String fileName = "com/hx/action/loginAction";
//		String pattern = "com/hx/*";				// true
//		String pattern = "com/?x/*";				// true
//		String pattern = "com/?/*";					// false
//		String pattern = "com/hx/action/login*";	// true
//		String pattern = "com/hx/action/login?";	// false
//		String pattern = "com/hx/action/*?";		// true
//		String pattern = "*/action/*?";				// true
		String pattern = "*/action/*/hx/*";			// false
		
		boolean isMatch = FileNameMatcher.match(fileName, pattern, true);
		Log.log(String.valueOf(isMatch) );
		
		// FileNameMatcher.preparePattern
//		String str = "com/hx/**?/abc/*????/sdf";
//		Log.log(FileNameMatcher.preparePattern(str) ); 
		
	}
	
}

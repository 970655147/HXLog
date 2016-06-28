/**
 * file name : Test08ForConstantsUtils.java
 * created at : ����6:15:00 2016��6��25��
 * created by 970655147
 */

package com.hx.log.test;

import com.hx.log.log.Log;
import com.hx.log.util.Constants;
import com.hx.log.util.ConstantsUtils;
import com.hx.log.util.Tools;

public class Test08ForConstantsUtils {

	// ����ConstantsUtils
	public static void main(String[] args) throws Exception {
		
		Log.log.logPatternChain = Constants.justPrintMsgLogPattern;
		
//		String codes = ConstantsUtils.generateCodesWithStaticFields(System.getProperty("user.dir") + "/src/HXLogConfig.conf");
//		Log.log(codes);
		
//		String codes = ConstantsUtils.generateCodesWithOpt(System.getProperty("user.dir") + "/src/HXLogConfig.conf");
//		Log.log(codes);
		
		
		String codes = ConstantsUtils.generateCodesWithOpt(Tools.getTmpPath("HXMongoConfig", ".conf") );
		Log.log(codes);
		
		
		
	}

}

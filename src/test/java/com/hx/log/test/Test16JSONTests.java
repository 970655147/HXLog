package com.hx.log.test;

import com.hx.log.json.JSONArray;
import com.hx.log.json.JSONObject;
import com.hx.log.util.Log;
import org.junit.Test;

import static com.hx.log.util.Log.info;

/**
 * Test16JSONTests
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/15/2017 4:08 PM
 */
public class Test16JSONTests {

    @Test
    public void JSONObjectFromObject() {

        Log.infoFatalLogger.logPatternChain = null;

//        String json = "{'name':'hx', \"age\":22}";
        String json = "{'name':'hx', \"age\":22, 'friends' : [1, 2, 4], 'chineseScore' : 70f, 'matchScore':56.4}";

        JSONObject obj = JSONObject.fromObject(json);

        info(obj);

        obj.element("abc", "sdf").element("ddd", 645d);
        obj.element("jsonObject", new JSONObject().element("a", "a").element("b", "b"));
        info(obj);

        System.out.println(obj.toString(4));
        System.out.println(obj.toString());

    }

    @Test
    public void fromObject02() {

        JSONArray arr = JSONArray.fromObject("[{\"title\":\"08两个线程交替打印121212...\",\"date\":\"2016-08-0921:43\",\"view\":\"(8)\"},{\"title\":\"17FileNameMatcher\",\"date\":\"2016-08-0721:20\",\"view\":\"(12)\"},{\"title\":\"16pointFixLike\",\"date\":\"2016-05-2122:00\",\"view\":\"(1841)\"},{\"title\":\"一件令人蛋疼的事\",\"date\":\"2016-04-0923:08\",\"view\":\"(89)\"},{\"title\":\"15几个Calender相关的方法\",\"date\":\"2016-03-0321:48\",\"view\":\"(103)\"},{\"title\":\"08scala,imported`Record'ispermanentlyhiddenbydefinitionofclassRecordinpackagetest\",\"date\":\"2016-03-0120:57\",\"view\":\"(161)\"},{\"title\":\"01SparkStreaming'sWordCount\",\"date\":\"2016-02-1221:49\",\"view\":\"(211)\"},{\"title\":\"14screenShotLikeQQ\",\"date\":\"2016-02-0220:53\",\"view\":\"(121)\"},{\"title\":\"13gifGenerator\",\"date\":\"2016-02-0120:36\",\"view\":\"(146)\"},{\"title\":\"12添加水印\",\"date\":\"2016-01-2721:08\",\"view\":\"(100)\"},{\"title\":\"11绘制雪花动态图\",\"date\":\"2016-01-2620:31\",\"view\":\"(102)\"},{\"title\":\"07八皇后问题\",\"date\":\"2016-01-2620:08\",\"view\":\"(104)\"},{\"title\":\"10绘制数字\",\"date\":\"2016-01-2521:05\",\"view\":\"(92)\"},{\"title\":\"30从n个数中随机获取m个数字\",\"date\":\"2016-01-2420:44\",\"view\":\"(387)\"},{\"title\":\"29同位词的统计\",\"date\":\"2016-01-2320:33\",\"view\":\"(89)\"}]");
        info(arr);

        System.out.println(arr.toString(4));
        System.out.println(arr.toString());

    }


}

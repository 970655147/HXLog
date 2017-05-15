/**
 * file name : FileUtils.java
 * created at : 22:02:05 2016-12-30
 * created by 970655147
 */

package com.hx.log.file;

import com.hx.common.interf.consumer.FileConsumer;
import com.hx.common.interf.consumer.StringConsumer;
import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.*;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import static com.hx.log.util.Log.err;
import static com.hx.log.util.Tools.assert0;

/**
 * 文件处理相关的工具
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:41 PM
 */
public final class FileUtils {

    /**
     * 默认的charset
     */
    public static String DEFAULT_CHARSET = Tools.DEFAULT_CHARSET;
    /**
     * 是否异步处理写操作
     */
    public static boolean WRITE_ASYNC = Tools.WRITE_ASYNC;

    // disable constructor
    private FileUtils() {
        assert0("can't instantiate !");
    }

    // ----------------- 文件操作相关方法 -----------------------

    /**
     * 判断是否需要打印日志
     *
     * @param logFlags 当前的log标志
     * @param logMask  给定的日志的mask
     * @return
     */
    public static boolean isLog(long logFlags, long logMask) {
        return ((logFlags & logMask) != 0);
    }

    // 将content字符串保存到指定的文件中
    // add 'isAsync' at 2016.04.16

    /**
     * 将给定的文本内容 保存到给定的文件
     *
     * @param content 给定的文本内容
     * @param file    需要保存的文件
     * @param charset 保存的字符集
     * @param isAsync 是否异步处理
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:42 PM
     * @since 1.0
     */
    public static void save(String content, File file, String charset, boolean isAsync) throws IOException {
        write(content, file, charset, isAsync, false);
    }

    public static void save(String content, File file, String charset) throws IOException {
        save(content, file, charset, WRITE_ASYNC);
    }

    public static void save(String content, File file, boolean isAsync) throws IOException {
        save(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void save(String content, File file) throws IOException {
        save(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    public static void save(String content, String file, String charset, boolean isAsync) throws IOException {
        save(content, new File(file), charset, isAsync);
    }

    public static void save(String content, String file, String charset) throws IOException {
        save(content, file, charset, WRITE_ASYNC);
    }

    public static void save(String content, String file, boolean isAsync) throws IOException {
        save(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void save(String content, String file) throws IOException {
        save(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    /**
     * 将给定的文本内容 追加到给定的文件
     *
     * @param content 给定的文本内容
     * @param file    需要保存的文件
     * @param charset 保存的字符集
     * @param isAsync 是否异步处理
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:44 PM
     * @since 1.0
     */
    public static void append(String content, File file, String charset, boolean isAsync) throws IOException {
        write(content, file, charset, isAsync, true);
    }

    public static void append(String content, File file, String charset) throws IOException {
        append(content, file, charset, WRITE_ASYNC);
    }

    public static void append(String content, File file, boolean isAsync) throws IOException {
        append(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void append(String content, File file) throws IOException {
        append(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    public static void append(String content, String file, String charset, boolean isAsync) throws IOException {
        append(content, new File(file), charset, isAsync);
    }

    public static void append(String content, String file, String charset) throws IOException {
        append(content, file, charset, WRITE_ASYNC);
    }

    public static void append(String content, String file, boolean isAsync) throws IOException {
        append(content, file, DEFAULT_CHARSET, isAsync);
    }

    public static void append(String content, String file) throws IOException {
        append(content, file, DEFAULT_CHARSET, WRITE_ASYNC);
    }

    // 1. could use 'tryWithResource' replace 'tryFinally'
    // 2. update 'BufferedOutputStream' with 'FileOutputStream' cause there need not 'Buffer'
    // at 2016.04.16

    /**
     * 将给定的内容 保存/添加 到给定的文件
     *
     * @param content  给定的文本内容
     * @param file     需要保存的文件
     * @param charset  保存的字符集
     * @param isAsync  是否异步处理
     * @param isAppend 是否追加
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:45 PM
     * @since 1.0
     */
    public static void write(final String content, final File file, final String charset,
                             boolean isAsync, final boolean isAppend) throws IOException {
        assert0(content != null, "'content' can't be null ");
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        Runnable writeTask = (new Runnable() {
            @Override
            public void run() {
                try (FileOutputStream fos = new FileOutputStream(file, isAppend)) {
                    fos.write(content.getBytes(charset));
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        });

        if (!isAsync) {
            writeTask.run();
        } else {
            Tools.execute(writeTask);
        }
    }

    public static void write(final String content, final File file, final String charset,
                             final boolean isAppend) throws IOException {
        write(content, file, charset, WRITE_ASYNC, isAppend);
    }

    public static void write(final String content, final File file, final boolean isAppend) throws IOException {
        write(content, file, DEFAULT_CHARSET, WRITE_ASYNC, isAppend);
    }

    /**
     * 给定的文件是否存在
     *
     * @param file 给定的文件
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:46 PM
     * @since 1.0
     */
    public static boolean exists(File file) {
        assert0(file != null, "'file' can't be null ");
        return file.exists();
    }

    public static boolean exists(String path) {
        assert0(path != null, "'path' can't be null ");
        return exists(new File(path));
    }

    /**
     * 重命名给定的文件
     *
     * @param src 原文件
     * @param dst 目标文件名
     * @return boolean
     * @author Jerry.X.He
     * @date 5/7/2017 2:35 PM
     * @since 1.0
     */
    public static boolean renameTo(File src, File dst) {
        if (!src.exists()) {
            Log.log("src file isn't exists !");
            return false;
        }
        if (dst.exists()) {
            Log.log("dst file is exists !");
            return false;
        }

        return src.renameTo(dst);
    }

    public static boolean renameTo(String src, String dst) {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");
        return renameTo(new File(src), new File(dst));
    }

    /**
     * 移除指定的文件
     *
     * @param file 需要移除的文件
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:46 PM
     * @since 1.0
     */
    public static void delete(File file) {
        assert0(file != null, "'path' can't be null ");

        if (file.exists()) {
            boolean isSucc = file.delete();
        }
    }

    public static void delete(String path) {
        assert0(path != null, "'path' can't be null ");
        delete(new File(path));
    }

    /**
     * 复制指定的文件
     *
     * @param src 源文件
     * @param dst 目标文件
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:47 PM
     * @since 1.0
     */
    public static void copy(File src, File dst) throws IOException {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");

        if (src.isDirectory()) {
            return;
        }
        if (!src.exists()) {
            return;
        }
        if (dst.exists()) {
            return;
        }

        FileInputStream fis = new FileInputStream(src);
        FileOutputStream fos = new FileOutputStream(dst);
        copy(fis, fos);
    }

    public static void copy(String src, String dst) throws IOException {
        assert0(src != null, "'src' can't be null ");
        assert0(dst != null, "'dst' can't be null ");
        copy(new File(src), new File(dst));
    }

    /**
     * 获取给定的输入流中的字符内容
     *
     * @param is      给定的字节流
     * @param charset 解码字节需要的编码
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:47 PM
     * @since 1.0
     */
    public static String getContent(InputStream is, String charset) throws IOException {
        assert0(is != null, "'inputStream' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        StringBuilder sb = new StringBuilder(is.available());
        try (BufferedReader br = new BufferedReader(new InputStreamReader(is, charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                sb.append(line);
                sb.append(Tools.CRLF);
            }
        }

        return sb.toString();
    }

    public static String getContent(InputStream is) throws IOException {
        return getContent(is, DEFAULT_CHARSET);
    }

    public static String getContent(File file, String charset) throws IOException {
        return getContent(new FileInputStream(file), charset);
    }

    public static String getContent(File file) throws IOException {
        return getContent(file, DEFAULT_CHARSET);
    }

    public static String getContent(String path, String charset) throws IOException {
        return getContent(new File(path), charset);
    }

    public static String getContent(String path) throws IOException {
        return getContent(new File(path), DEFAULT_CHARSET);
    }

    /**
     * 获取文件的所有的行, 存储在一个结果的List, 文件过大, 慎用此方法
     *
     * @param file         给定的文件
     * @param charset      解码字节需要的编码
     * @param estimateSize 估量文件的行数
     * @return java.util.List<java.lang.String>
     * @author Jerry.X.He
     * @date 5/4/2017 11:48 PM
     * @since 1.0
     */
    public static List<String> getContentWithList(File file, String charset, int estimateSize) throws IOException {
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");

        List<String> lines = new ArrayList<>(estimateSize);
        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                lines.add(line);
            }
        }

        return lines;
    }

    public static List<String> getContentWithList(File file, int estimateSize) throws IOException {
        return getContentWithList(file, DEFAULT_CHARSET, estimateSize);
    }

    public static List<String> getContentWithList(File file, String charset) throws IOException {
        return getContentWithList(file, charset, Tools.ESTIMATE_FILE_LINES);
    }

    public static List<String> getContentWithList(File file) throws IOException {
        return getContentWithList(file, DEFAULT_CHARSET);
    }

    public static List<String> getContentWithList(String file, String charset, int estimateSize) throws IOException {
        return getContentWithList(new File(file), charset, estimateSize);
    }

    public static List<String> getContentWithList(String file, int estimateSize) throws IOException {
        return getContentWithList(new File(file), estimateSize);
    }

    public static List<String> getContentWithList(String file, String charset) throws IOException {
        return getContentWithList(new File(file), charset);
    }

    public static List<String> getContentWithList(String file) throws IOException {
        return getContentWithList(new File(file));
    }

    /**
     * 使用给定的消费者消费指定的文件的每一行
     *
     * @param file     给定的文件
     * @param charset  给定的字符集
     * @param consumer 给定的消费者
     * @return java.util.List<java.lang.String>
     * @author Jerry.X.He
     * @date 5/12/2017 9:13 PM
     * @since 1.0
     */
    public static <T> T consumeContentList(File file, String charset, StringConsumer<T> consumer) throws IOException {
        assert0(file != null, "'file' can't be null ");
        assert0(charset != null, "'charset' can't be null ");
        assert0(consumer != null, "'consumer' can't be null ");

        try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset))) {
            String line = null;
            while ((line = br.readLine()) != null) {
                consumer.consume(line);
            }
        }
        return consumer.get();
    }

    public static <T> T consumeContentList(File file, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(file, DEFAULT_CHARSET, consumer);
    }

    public static <T> T consumeContentList(String file, String charset, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(new File(file), charset, consumer);
    }

    public static <T> T consumeContentList(String file, StringConsumer<T> consumer) throws IOException {
        return consumeContentList(new File(file), consumer);
    }

    /**
     * 从指定的url上面下载图片  保存到指定的路径下面 [也适用于下载其他的二进制数据]
     *
     * @param urlStr 给定的url
     * @param path   需要保存的文件路径
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:48 PM
     * @since 1.0
     */
    public static void downloadFrom(String urlStr, String path) throws IOException {
        assert0(urlStr != null, "'urlStr' can't be null ");
        assert0(path != null, "'path' can't be null ");

        URL url = new URL(urlStr);
        InputStream is = url.openStream();
        OutputStream os = new FileOutputStream(new File(path));
        copy(is, os);
    }

    /**
     * 将输入流中的数据 复制到输出流
     *
     * @param is            给定的输入流
     * @param os            给定的输出流
     * @param isCloseStream 是否需要关闭流
     * @return void
     * @author Jerry.X.He
     * @date 5/4/2017 11:49 PM
     * @since 1.0
     */
    public static void copy(InputStream is, OutputStream os, boolean isCloseStream) {
        assert0(is != null, "'inputStream' can't be null ");
        assert0(os != null, "'outputStream' can't be null ");

        BufferedInputStream bis = null;
        BufferedOutputStream bos = null;
        try {
            bis = new BufferedInputStream(is);
            bos = new BufferedOutputStream(os);
            int len = 0;
            byte[] buf = new byte[Tools.BUFF_SIZE_ON_TRANS_STREAM];
            while ((len = bis.read(buf)) != -1) {
                bos.write(buf, 0, len);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (isCloseStream) {
                if (bos != null) {
                    try {
                        bos.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                if (bis != null) {
                    try {
                        bis.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

    public static void copy(InputStream is, OutputStream os) {
        copy(is, os, true);
    }

    /**
     * 获取指定路径的文件的文件, 通过sep分割的文件名     获取文件名
     * 解析? 的位置, 是为了防止一下情况
     *
     * @param path 给定的文件路径
     * @param sep  文件路径分隔符
     * @return java.lang.String
     * @author Jerry.X.He
     * @date 5/4/2017 11:49 PM
     * @since 1.0
     */
    public static String getFileName(String path, char sep) {
        assert0(path != null, "'path' can't be null ");
        int start = path.lastIndexOf(sep) + 1;

//		http://webd.home.news.cn/1.gif?z=1&_wdxid=01002005057000300000000001110
        int end = getSymAfterFileName(path, start + 1);
        if (end != -1) {
            return path.substring(start, end);
        } else {
            return path.substring(start);
        }
    }

    /**
     * 消费给定的文件(夹), 递归处理
     *
     * @param file     给定的文件
     * @param consumer 给定的文件消费者
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 10:20 PM
     * @since 1.0
     */
    public static <T> T traverseFiles(File file, FileConsumer<T> consumer) {
        Tools.assert0(file != null, "'file' can't be null !");
        Tools.assert0(consumer != null, "'consumer' can't be null !");

        if (!file.exists()) {
            err("please makeSure " + file.getPath() + " does exists ...");
            return consumer.get();
        }

        consumer.consume(file);
        if (file.isDirectory()) {
            File[] files = file.listFiles();
            if (!Tools.isEmpty(files)) {
                for (File childFile : files) {
                    traverseFiles(childFile, consumer);
                }
            }
        }

        return consumer.get();
    }

    public static <T> T traverseFiles(String folder, FileConsumer<T> consumer) {
        Tools.assert0(folder != null, "'folder' can't be null !");
        return traverseFiles(new File(folder), consumer);
    }


    // ----------------- 辅助方法 -----------------------

    /**
     * 获取文件名后面的可能出现的符合的最近的索引
     *
     * @param path  给定的路径
     * @param start 最后一个文件分隔符的位置+1
     * @return int
     * @author Jerry.X.He
     * @date 5/4/2017 11:50 PM
     * @since 1.0
     */
    private static int getSymAfterFileName(String path, int start) {
        int min = -1;
        for (int i = start; i < path.length(); i++) {
            if (Tools.MAYBE_FILE_NAME_SEPS.contains(path.charAt(i))) {
                min = i;
                break;
            }
        }

        return min;
    }

}
